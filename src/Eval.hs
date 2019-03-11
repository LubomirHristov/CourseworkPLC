module Eval where
import Grammar

eval1 :: Exp -> [[Int]] -> [[Int]]
eval1 (MyTokenAppend n e) xs = [n]: (eval1 e xs)
eval1 (MyFinalTokenAppend n) xs = [n]:xs
eval1 (MyTokenPrint e) xs = eval1 e xs
--eval1 TokenInput1 (x:xs) = xs
