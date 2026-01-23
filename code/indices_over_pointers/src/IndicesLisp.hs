{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module IndicesLisp (repl) where


import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import System.IO
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV


-- Now instead of pointers we use indices
newtype NumIdx = NumIdx       { unNumIdx    :: Integer }
newtype StringIdx = StringIdx { unStringIdx :: Int }
newtype ExprIdx = ExprIdx     { unExprIdx   :: Int }
newtype BuiltinIdx = BuiltinIdx { unExprIdx   :: Int }

type Buf s = MV.MVector s Expr
data Arena s = Arena { pool :: Buf s -- the backing buffer
                     , next :: Int   -- the next free slot in the arena
                     }

newArena :: Int -> ST s ( s)
newArena size = do
  buf <- MV.new size
  pure $ Arena { pool = buf, next = 0 }

alloc :: Arena s -> Int -> Expr -> ST s ExprIdx
alloc Arena{..} !ix expr = do
  MV.write pool ix expr
  pure (ExprIdx ix) -- return the handle to the expr

lookup :: Arena s -> ExprIdx -> Expr
lookup Arena{..} ix = pool V.! ix

-- now our AST is half the size because each indice is 4 bytes rather than a
-- machine word (8 bytes on x86_64).
data Expr
  = Number  !NumIdx
  | Symbol  !StringIdx
  | List    !ValIdx
  | Builtin !FunIdx
  | Lambda  !StringIdx !ValIdx !EnvIdx

instance Show Expr where
  show = \case
    Number n -> show n
    Symbol s -> s
    List xs -> "(" ++ unwords (map show xs) ++ ")"
    Func _ -> "<builtin>"
    Lambda {} -> "<lambda>"

-- Exprs
type Env = IM.Map ExprIdx      -- Benefit 2: now env can be keyed on indices
                               -- rather than Strings. This could even be an
                               -- array.

-- When using this approach we want to scope our memory to the component in our
-- system. In this case that component is the Evaluator
data EvalState = EvalState { evalArena :: Arena }

-- Now we define the monad for our component. I chose to use the ReaderT over IO
-- (well, ST) pattern here because the underlying vector is mutable.
newtype Eval a = Eval { runEval :: ReaderT EvalState (ST s) a }
  deriving (Functor, Applicative, Monad, MonadReader EvalState, MonadIO)

-- Now to Eval. First notice the type...TODO
eval :: Env -> ExprIdx -> Eval ExprIdx
eval env e_ix = do
  arena <- asks evalArena
  case lookup arena e_ix of
    Number n -> Right

-- eval env !e_idx = \case
--   Number n -> Right (Number n)
--   Symbol s ->
--     maybe (Left $ "unbound symbol: " ++ s) Right (M.lookup s env)

--   List [Symbol "quote", x] ->
--     Right x

--   List [Symbol "if", cond, t, f] -> do
--     v <- eval env cond
--     case v of
--       Number 0 -> eval env f
--       _        -> eval env t

--   List [Symbol "define", Symbol name, expr] -> do
--     val <- eval env expr
--     Right val

--   List (Symbol "lambda" : List params : body : []) ->
--     Right $ Lambda [ p | Symbol p <- params ] body env

--   List (fn : args) -> do
--     f <- eval env fn
--     xs <- mapM (eval env) args
--     apply f xs

--   bad -> Left $ "cannot eval: " ++ show bad

apply :: Value -> [Value] -> Eval Value
apply = \case
  Func f -> f
  Lambda params body clo ->
    \args ->
      if length params /= length args
        then Left "arity mismatch"
        else eval (M.union (M.fromList (zip params args)) clo) body
  _ -> const $ Left "not a function"

-- === Builtins ===

builtins :: Env
builtins = M.fromList
  [ ("+", numBinOp (+))
  , ("-", numBinOp (-))
  , ("*", numBinOp (*))
  , ("/", numBinOp div)
  , ("=", numCmpOp (==))
  ]

numBinOp :: (Integer -> Integer -> Integer) -> Value
numBinOp op = Func $ \case
  [Number a, Number b] -> Right (Number (a `op` b))
  _ -> Left "expected two numbers"

numCmpOp :: (Integer -> Integer -> Bool) -> Value
numCmpOp op = Func $ \case
  [Number a, Number b] ->
    Right (Number (if a `op` b then 1 else 0))
  _ -> Left "expected two numbers"

-- === REPL ===

repl :: Env -> IO ()
repl env = do
  putStr "lisp> "
  hFlush stdout
  eof <- isEOF
  unless eof $ do
    line <- getLine
    unless (null line) $
      case parse line >>= eval env of
        Left err -> putStrLn ("error: " ++ err)
        Right v  -> print v
    repl env

main :: IO ()
main = do
  putStrLn "Mini Lisp (Ctrl-D to quit)"
  repl builtins
