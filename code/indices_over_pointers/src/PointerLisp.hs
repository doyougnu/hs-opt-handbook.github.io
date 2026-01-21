{-# LANGUAGE LambdaCase #-}

module PointerLisp (repl) where


import Control.Monad
import Data.Char
import qualified Data.Map as M
import System.IO


data Value
  = Number Int
  | Symbol String
  | List [Value]
  | Func ([Value] -> Eval Value)
  | Lambda [String] Expr Env

instance Show Value where
  show = \case
    Number n -> show n
    Symbol s -> s
    List xs -> "(" ++ unwords (map show xs) ++ ")"
    Func _ -> "<builtin>"
    Lambda {} -> "<lambda>"

-- Exprs
type Expr = Value
type Env = M.Map String Value
type Eval a = Either String a

-- tiny parser
parse :: String -> Either String Expr
parse s =
  case readExpr (tokenize s) of
    Just (e, []) -> Right e
    _            -> Left "parse error"

tokenize :: String -> [String]
tokenize = words . concatMap f
  where
    f '(' = " ( "
    f ')' = " ) "
    f c   = [c]

readExpr :: [String] -> Maybe (Expr, [String])
readExpr = \case
  [] -> Nothing
  "(" : xs -> readList xs
  ")" : _  -> Nothing
  tok : xs -> Just (atom tok, xs)

readList :: [String] -> Maybe (Expr, [String])
readList = go []
  where
    go acc = \case
      []       -> Nothing
      ")" : xs -> Just (List (reverse acc), xs)
      xs       -> do
        (e, xs') <- readExpr xs
        go (e : acc) xs'

atom :: String -> Expr
atom s
  | all isDigit s = Number (read s)
  | otherwise     = Symbol s

-- === Evaluation ===

eval :: Env -> Expr -> Eval Value
eval env = \case
  Number n -> Right (Number n)
  Symbol s ->
    maybe (Left $ "unbound symbol: " ++ s) Right (M.lookup s env)

  List [Symbol "quote", x] ->
    Right x

  List [Symbol "if", cond, t, f] -> do
    v <- eval env cond
    case v of
      Number 0 -> eval env f
      _        -> eval env t

  List [Symbol "define", Symbol name, expr] -> do
    val <- eval env expr
    Right val

  List (Symbol "lambda" : List params : body : []) ->
    Right $ Lambda [ p | Symbol p <- params ] body env

  List (fn : args) -> do
    f <- eval env fn
    xs <- mapM (eval env) args
    apply f xs

  bad -> Left $ "cannot eval: " ++ show bad

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
