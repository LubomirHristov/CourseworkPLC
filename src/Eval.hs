module Eval where
import Grammar

eval1 :: Exp -> [[Int]] -> [[Int]]
eval1 (TokenPrintconcat n) xs = [n]:xs
--eval1 TokenInput1 (x:xs) = xs
