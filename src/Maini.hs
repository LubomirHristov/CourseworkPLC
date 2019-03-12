import Tokens
import Grammar
import Eval
import System.Environment
import Control.Exception
import System.IO


main = do
           input <- readFile "input.txt"
           -- sourceText <- readFile "program.txt"
           -- putStrLn ("Parsing : " ++ sourceText)
           -- let parsedProg = parseCalc (alexScanTokens "input")
           -- putStrLn ("Parsed as " ++ (show parsedProg) ++ "\n")
           -- -- putStrLn ("Type Checking : " ++ (show parsedProg) ++ "\n")
           -- -- let typedProg = typeOf [] parsedProg
           -- -- putStrLn ("Type Checking Passed with type " ++ (unparseType typedProg) ++ "\n")
           -- putStrLn("-----------------------------------------")
           -- let result = eval1 (parsedProg) (convert input 0 [])
           return input
          -- putStrLn ("Evaluates to " ++ (unparse result) ++ "\n")
--
-- convert :: String -> Int -> [[Int]] -> [[Int]]
-- convert s index acc
--                     | index == (length (lines s)) = acc
--                     | otherwise = convert s (index+1) (acc ++ [ints])
--                           where
--                             arr = lines s
--                             ints = map read (words (arr !! index))  :: [Int]
