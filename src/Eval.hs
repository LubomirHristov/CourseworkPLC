module Eval where
import Grammar
import Data.Char

--       exp    curr_in     curr_out    line      out  , operation
eval1 :: Exp -> [[Int]] -> [[Int]] -> [Int] -> ([Int], String)
eval1 (MyTokenNum n) input out _ = ([n],"")

eval1 (MyTokenVar v) input out xs = ([entry1],"")
      where
        entry1 = xs !! index1
        index1 = extract "" v

eval1 (MyTokenLimit n) input out _ = ([],(show n))

eval1(MyTokenVarOp v (MyTokenEmptyPrepend)) input out xs = ([], "emptyPrepend")

eval1 (MyTokenVarOp v (MyTokenPrepend e)) input out xs | length out == 0 = (fst(eval1 e input out xs), "prepend")
                                                       | otherwise = ([(xs !! index)], "")
      where index = extract "" v

eval1 (MyTokenIn) input out xs = ([sum(concat input)], "")

eval1 (MyTokenOut) input out xs = ([sum(concat out)], "")

eval1 (MyTokenSum e) input out xs = ([sum(fst (eval1 e input out xs))],(snd (eval1 e input out xs)))

eval1 (MyTokenOutArr e) input out xs | element >= length out = ([0],"")
                                     | otherwise = ([(head(out !! element))],"")
      where element = head(fst(eval1 e input out xs))

eval1 (MyTokenVarOp v (MyTokenCopy)) input out xs = ([(xs !! index),(xs !! index)], "copy")
      where index = extract "" v

eval1 (MyTokenVarOp v (MyTokenPlusNum n)) input out xs = ([(xs !! index) + n],"")
      where index = extract "" v

eval1 (MyTokenVarOp v (MyTokenMinusNum n)) input out xs = ([(xs !! index) - n], "")
      where index = extract "" v

eval1 (MyTokenVarOp v (MyTokenTimesNum n)) input out xs = ([(xs !! index) * n], "")
      where index = extract "" v

eval1 (MyTokenVarOp v (MyTokenDivNum n)) input out xs = ([(xs !! index) `div` n], "")
      where index = extract "" v

eval1 (MyTokenStreamOp v1 (MyTokenPlus) (MyTokenVar v2)) input out xs = ([entry1 + entry2], "")
      where
        entry1 = xs !! index1
        entry2 = xs !! index2
        index1 = extract "" v1
        index2 = extract "" v2

eval1 (MyTokenStreamOp v (MyTokenPlus) (MyTokenInArr e)) input out xs | length input == 0 = ([entry1], "")
                                                                      | otherwise = ([entry1 + ((input !! index1) !! element)],(show input))
                                                                          where
                                                                            entry1 = xs !! index1
                                                                            index1 = extract "" v
                                                                            element = head (fst(eval1 e input out xs))

-- eval1 (MyTokenStreamOp v (MyTokenPlus) (MyTokenOutArr e)) input out xs | length out < 2 = ([entry1], "")
--                                                                        | otherwise = ([entry1 + (head(out !! (head (fst(eval1 e input out xs)))))], "")
--                                                                           where
--                                                                             entry1 = xs !! index1
--                                                                             index1 = extract "" v

eval1 (MyTokenStreamOp v (MyTokenPlus) e) input out xs = ([entry + head (fst(eval1 e input out xs))], (snd (eval1 e input out xs)))
      where
        entry = xs !! index
        index = extract "" v

eval1 (MyTokenStreamOp v1 (MyTokenMinus) (MyTokenVar v2)) input out xs = ([entry1 - entry2], "")
      where
        entry1 = xs !! index1
        entry2 = xs !! index2
        index1 = extract "" v1
        index2 = extract "" v2

eval1 (MyTokenStreamOp v (MyTokenMinus) (MyTokenOutArr e)) input out xs | length out == 0 = ([entry1], "")
                                                                 | length out < length(fst(eval1 e input out xs)) = ([entry1 - ((last out) !! index1)], "")
                                                                 | otherwise = ([entry1 - sum(extractFromOut index1 out (fst(eval1 e input out xs)))], "")
                                                                    where
                                                                      entry1 = xs !! index1
                                                                      index1 = extract "" v

eval1 (MyTokenStreamOp v (MyTokenMinus) e) input out xs = ([entry - head (fst(eval1 e input out xs))], "")
      where
        entry = xs !! index
        index = extract "" v

eval1 (MyTokenStreamOp v1 (MyTokenTimes) (MyTokenVar v2)) input out xs = ([entry1 * entry2], "")
      where
        entry1 = xs !! index1
        entry2 = xs !! index2
        index1 = extract "" v1
        index2 = extract "" v2


eval1 (MyTokenStreamOp v (MyTokenTimes) (MyTokenOutArr e)) input out xs | length out == 0 = ([entry1], "")
                                                                 | length out < length(fst(eval1 e input out xs)) = ([entry1 * ((last out) !! index1)], "")
                                                                 | otherwise = ([entry1 * product(extractFromOut index1 out (fst(eval1 e input out xs)))], "")
                                                                    where
                                                                      entry1 = xs !! index1
                                                                      index1 = extract "" v


eval1 (MyTokenStreamOp v (MyTokenTimes) e) input out xs = ([entry * head (fst(eval1 e input out xs))], "")
      where
        entry = xs !! index
        index = extract "" v

eval1 (MyTokenStreamOp v1 (MyTokenDiv) (MyTokenVar v2)) input out xs = ([entry1 `div` entry2], "")
      where
        entry1 = xs !! index1
        entry2 = xs !! index2
        index1 = extract "" v1
        index2 = extract "" v2

eval1 (MyTokenStreamOp v (MyTokenDiv) e) input out xs = ([entry `div` head (fst(eval1 e input out xs))], "")
      where
        entry = xs !! index
        index = extract "" v

eval1 (MyTokenExpOperation e1 (MyTokenPlus) e2) input out xs | fst(eval1 e1 input out xs) == [] = ([head(fst(eval1 e2 input out xs))],"")
                                                             | otherwise = ([head(fst(eval1 e1 input out xs)) + head(fst(eval1 e2 input out xs))],"")

eval1 (MyTokenExpOperation e1 (MyTokenMinus) e2) input out xs = ([(head(fst(eval1 e1 input out xs)) - head(fst(eval1 e2 input out xs)))],"")

eval1 (MyTokenExpOperation e1 (MyTokenTimes) e2) input out xs = ([(head(fst(eval1 e1 input out xs)) * head(fst(eval1 e2 input out xs)))],"")

eval1 (MyTokenExpOperation e1 (MyTokenDiv) e2) input out xs = ([(head(fst(eval1 e1 input out xs)) `div` head(fst(eval1 e2 input out xs)))],"")

eval1 (MyTokenSeparator e1 e2) input out xs = (fst (eval1 e1 input out xs) ++ fst (eval1 e2 input out xs), (snd (eval1 e1 input out xs)))

extract :: String -> String -> Int
extract acc [] = read acc
extract acc (x:xs) | isDigit x = extract (acc ++ [x]) xs
                   | otherwise = extract acc xs

extractFromOut :: Int -> [[Int]] -> [Int] -> [Int]
extractFromOut index out [] = []
extractFromOut index out (x:xs) = ((out !! x) !! index) : extractFromOut index out xs

getStreamOutSoFar :: [[Int]] -> Int -> Int
getStreamOutSoFar [] _ = 0
getStreamOutSoFar (x:xs) index = (x !! index) + getStreamOutSoFar xs index
