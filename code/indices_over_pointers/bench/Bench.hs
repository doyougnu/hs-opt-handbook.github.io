module Main where

import qualified Test.Tasty.Bench as B
import Control.Exception (evaluate)
import Control.DeepSeq (force)

import qualified PointerLisp as PL

main :: IO ()
main = B.defaultMain
  [ B.bgroup "Pointer Lisp"
    [ bench 10
    , bench 100
    , bench 1000
    , bench 10000
    ]
  ]


bench :: Integer -> B.Benchmark
bench n =
  B.bench ("(fact " ++ show n ++ ")") $ B.nf PL.mkFactProgram n
