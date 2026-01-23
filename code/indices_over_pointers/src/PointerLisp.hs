{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module PointerLisp where


import Control.DeepSeq
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import GHC.Generics

import Debug.Trace

data Expr
  = Number Integer
  | Symbol String
  | List [Expr]
  | Lambda [Expr] Expr Env
  | Builtin ([Expr] -> Expr)
  deriving (Generic, NFData)

instance Show Expr where
  show = \case
    Number n -> show n
    Symbol s -> s
    List xs -> "(" ++ unwords (map show xs) ++ ")"
    Builtin _ -> "<builtin>"
    Lambda {} -> "<lambda>"

fact :: Expr
fact = Lambda args body env
  where
    args = pure $ List [Symbol "n"]
    body = List [Symbol "if", cond, thn, els]
    cond = List [Symbol "=", Symbol "n", Number 0]
    thn  = Number 1
    els  = List [ Symbol "*"
                , Symbol "n"
                , List [ Symbol "fact"
                       , List [ Symbol "-"
                              , Symbol "n"
                              , Number 1
                              ]
                       ]
                ]
    env = M.insert "fact" fact builtins

mkFactProgram :: Integer -> Eval Expr
mkFactProgram n = apply fact [Number n]

-- Exprs
type Env = M.Map String Expr
type Eval a = Either String a

atom :: String -> Expr
atom s
  | all isDigit s = Number (read s)
  | otherwise     = Symbol s

eval :: Env -> Expr -> Eval Expr
eval env = \case
  Number n -> Right (Number n)
  Symbol s -> maybe (Left $ "unbound symbol: " ++ s) Right (M.lookup s env)
  List [Symbol "quote", x] -> Right x

  List [Symbol "if", cond, t, f] -> do
    v <- eval env cond
    case v of
      Number 0 -> eval env f
      _        -> eval env t

  List [Symbol "define", Symbol name, expr] -> do
    val <- eval env expr
    Right val

  List (Symbol "lambda" : List params : body : []) ->
    Right $ Lambda [ Symbol p | Symbol p <- params ] body env

  List (fn : args) -> do
    f <- eval env fn
    xs <- mapM (eval env) args
    apply f xs

  bad -> Left $ "cannot eval: " ++ show bad

symToString :: Expr -> Maybe String
symToString (List [Symbol s]) = Just s
symToString _          = Nothing

apply :: Expr -> [Expr] -> Eval Expr
apply =
  let die_arity = Left "arity mismatch"
      die_type  =  Left "not a function"
  in \case
    Builtin f -> Right . f
    Lambda params' body clo ->
      \args ->
        let params = catMaybes $ fmap symToString params'
            env    = M.union (M.fromList (zip params args)) clo
        in
          if  length params /= length args
          then die_arity
          else eval env body
    _ -> const die_type


builtins :: Env
builtins = M.fromList
  [ ("+", numBinOp (+))
  , ("-", numBinOp (-))
  , ("*", numBinOp (*))
  , ("/", numBinOp div)
  , ("=", numCmpOp (==))
  ]

numBinOp :: (Integer -> Integer -> Integer) -> Expr
numBinOp op = Builtin $ \case
  [Number a, Number b] -> Number (a `op` b)
  _ -> error "numBinOp: bad args"

numCmpOp :: (Integer -> Integer -> Bool) -> Expr
numCmpOp op = Builtin $ \case
  [Number a, Number b] ->  Number (if a `op` b then 1 else 0)
  _ -> error "numCmpOp: bad args"
