import Tokens
import Grammar
import Eval
import System.Environment
import Control.Exception
import System.IO
import Data.Char

main :: IO()
main = do
          (fileName : _) <- getArgs
          sourceText <- readFile fileName
          let parsedProg = parseCalc (alexScanTokens sourceText)
          mainloop parsedProg []

mainloop :: Exp -> [[Int]] -> IO()
mainloop parsedProg out = do
                            ineof <- isEOF
                            if ineof
                                then do return()
                                else do
                                        line <- getLine
                                        let result = eval1 parsedProg out (convert line)
                                        if(isMyNumber (snd result)) --if snd result is the limit
                                          then do
                                            if (read (snd result) == length out) --if limit is reached
                                              then do return ()
                                              else do --else continue reading lines
                                                execute parsedProg result line out
                                          else do --else continue reading lines
                                                execute parsedProg result line out


--convert one line of the input
convert :: String -> [Int]
convert s = map read (words s)

--prints the current line that has to be outputted
myprint :: [Int] -> IO()
myprint [] = return ()
myprint (x:xs) = do
                  putStr((show x) ++ " ")
                  myprint xs

--checks if the string is a number
isMyNumber :: String -> Bool
isMyNumber [] = False
isMyNumber (x:[]) | isDigit x = True
                  | otherwise = False
isMyNumber (x:xs) | (isDigit x) = isMyNumber xs
                  | otherwise = False

--prints a simple array element by element
printArray :: [Int] -> IO()
printArray [] = return ()
printArray (x:xs) = do
                      putStr((show x) ++ " ")
                      printArray xs

--prints the numbers that have to be prepended (handles multiple streams and ultiple appends)
{-
  @parameters
  acc - stores the numbers that don't have to be printed on the current iteration
  (x:xs) - the list to work on
  numStream - the number of streams
  index - current index of the list
  len - original length of the list
-}
printAppend :: [Int] -> [Int] -> Int -> Int -> Int -> IO()
printAppend acc (x:xs) numStream index len
                                          | len == numStream = printArray (x:xs)
                                          | index == len - 1 = do
                                                                  putStr("\n")
                                                                  --add the last number to the acc and => increase its length by 1
                                                                  printAppend [] (acc ++ [x]) numStream 0 ((length acc) + 1)
                                          | index `mod` (len `div` numStream) == 0 = do
                                                                                        putStr((show x) ++ " ")
                                                                                        --print elem and increase index
                                                                                        printAppend acc xs numStream (index+1) len
                                          | otherwise = printAppend (acc ++ [x]) xs numStream (index+1) len --just store the current element

--a helper function that continues evaluating the input
execute :: Exp -> ([Int],String) -> String -> [[Int]] -> IO()
execute parsedProg result line out = do
                              if((snd result) == "append") --if we have to append
                                then do
                                        printAppend [] (fst result) (length (convert line)) 0 (length (fst result))
                                        putStr("\n")
                                        putStr (line)
                                        putStr("\n")
                                        mainloop parsedProg ([fst result] ++ out)
                                else do --else just print the simple output
                                  myprint (fst result)
                                  putStr("\n")
                                  mainloop parsedProg ([fst result] ++ out)
