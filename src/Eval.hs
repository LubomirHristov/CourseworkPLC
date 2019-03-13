module Eval where
import Grammar

--the Bool says if operations should continue
eval1 :: Exp -> [Int] -> ([[Int]], Bool)
--eval1 (MyTokenAppend n e) xs = ([n]: fst (eval1 e xs), False)
--eval1 (MyFinalTokenAppend n) xs = ([n]:[xs], False)
eval1 (MyTokenPrint e) xs = (fst (eval1 e xs), snd (eval1 e xs))
eval1 MyTokenCopy xs = ([xs ++ xs],True)
--eval1 TokenInput1 (x:xs) = xs
