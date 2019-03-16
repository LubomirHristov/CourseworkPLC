module Eval where
import Grammar
import Data.Char

--       exp    currout    line     out
eval1 :: Exp -> [[Int]] -> [Int] -> ([Int], String)
eval1 (MyTokenNum n) out _ = ([n],"")

eval1 (MyTokenVar v) out _ = ([extract "" v],"")

eval1 (MyTokenLimit n) out _ = ([],(show n))

eval1 (MyTokenVarOp v (MyTokenAppend n)) out xs | length out == 0 = ([n], "append")
                                                | otherwise = ([(xs !! index)], "")
                                                    where index = extract "" v

eval1 (MyTokenOutArr e) out xs = (numbers,"")
      where numbers = extractFromOut out (fst(eval1 e out xs))

eval1 (MyTokenVarOp v (MyTokenCopy)) out xs = ([(xs !! index),(xs !! index)], "copy")
      where index = extract "" v

eval1 (MyTokenVarOp v (MyTokenPlusNum n)) out xs = ([(xs !! index) + n],"")
      where index = extract "" v

eval1 (MyTokenVarOp v (MyTokenMinusNum n)) out xs = ([(xs !! index) - n], "")
      where index = extract "" v

eval1 (MyTokenVarOp v (MyTokenTimesNum n)) out xs = ([(xs !! index) * n], "")
      where index = extract "" v

eval1 (MyTokenVarOp v (MyTokenDivNum n)) out xs = ([(xs !! index) `div` n], "")
      where index = extract "" v

eval1 (MyTokenStreamOp v1 (MyTokenPlus) (MyTokenVar v2)) out xs = ([entry1 + entry2], "")
      where
        entry1 = xs !! index1
        entry2 = xs !! index2
        index1 = extract "" v1
        index2 = extract "" v2

eval1 (MyTokenStreamOp v (MyTokenPlus) (MyTokenOutArr e)) out xs | length out == 0 = ([entry1], "")
                                                                 | length out < length(fst(eval1 e out xs)) = ([entry1 + head(last out)], "")
                                                                 | otherwise = ([entry1 + sum(fst(eval1 (MyTokenOutArr e) out xs))], "")
                                                                    where
                                                                      entry1 = xs !! index1
                                                                      index1 = extract "" v

eval1 (MyTokenStreamOp v (MyTokenPlus) e) out xs = ([entry + head (fst(eval1 e out xs))], "")
      where
        entry = xs !! index
        index = extract "" v

eval1 (MyTokenStreamOp v1 (MyTokenMinus) (MyTokenVar v2)) out xs = ([entry1 - entry2], "")
      where
        entry1 = xs !! index1
        entry2 = xs !! index2
        index1 = extract "" v1
        index2 = extract "" v2

eval1 (MyTokenStreamOp v (MyTokenMinus) (MyTokenOutArr e)) out xs | length out == 0 = ([entry1], "")
                                                                 | length out < length(fst(eval1 e out xs)) = ([entry1 - head(last out)], "")
                                                                 | otherwise = ([entry1 - sum(fst(eval1 (MyTokenOutArr e) out xs))], "")
                                                                    where
                                                                      entry1 = xs !! index1
                                                                      index1 = extract "" v



eval1 (MyTokenStreamOp v (MyTokenMinus) e) out xs = ([entry - head (fst(eval1 e out xs))], "")
      where
        entry = xs !! index
        index = extract "" v

eval1 (MyTokenStreamOp v1 (MyTokenTimes) (MyTokenVar v2)) out xs = ([entry1 * entry2], "")
      where
        entry1 = xs !! index1
        entry2 = xs !! index2
        index1 = extract "" v1
        index2 = extract "" v2


eval1 (MyTokenStreamOp v (MyTokenTimes) (MyTokenOutArr e)) out xs | length out == 0 = ([entry1], "")
                                                                 | length out < length(fst(eval1 e out xs)) = ([entry1 * head(last out)], "")
                                                                 | otherwise = ([entry1 * product(fst(eval1 (MyTokenOutArr e) out xs))], "")
                                                                    where
                                                                      entry1 = xs !! index1
                                                                      index1 = extract "" v


eval1 (MyTokenStreamOp v (MyTokenTimes) e) out xs = ([entry * head (fst(eval1 e out xs))], "")
      where
        entry = xs !! index
        index = extract "" v

eval1 (MyTokenStreamOp v1 (MyTokenDiv) (MyTokenVar v2)) out xs = ([entry1 `div` entry2], "")
      where
        entry1 = xs !! index1
        entry2 = xs !! index2
        index1 = extract "" v1
        index2 = extract "" v2

eval1 (MyTokenStreamOp v (MyTokenDiv) e) out xs = ([entry `div` head (fst(eval1 e out xs))], "")
      where
        entry = xs !! index
        index = extract "" v

eval1 (MyTokenSeparator e1 e2) out xs = (fst (eval1 e1 out xs) ++ fst (eval1 e2 out xs), (snd (eval1 e1 out xs)))


extract :: String -> String -> Int
extract acc [] = read acc
extract acc (x:xs) | isDigit x = extract (acc ++ [x]) xs
                   | otherwise = extract acc xs

extractFromOut :: [[Int]] -> [Int] -> [Int]
extractFromOut out [] = []
extractFromOut out (x:xs) = head (out !! x) : extractFromOut out xs
