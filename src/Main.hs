import Tokens
import Grammar
import Eval
import System.Environment
import Control.Exception
import System.IO
import Data.Char
import Data.List
import Data.Maybe

main :: IO()
main = do
          [fileName , input ] <- getArgs
          inh <- openFile input ReadMode
          sourceText <- readFile fileName
          let parsedProg = parseCalc (alexScanTokens sourceText)
          mainloop parsedProg inh []
          hClose inh

mainloop :: Exp -> Handle -> [[Int]] -> IO()
mainloop parsedProg inh out = do
                 ineof <- hIsEOF inh
                 if ineof
                        then do
                              return ()
                        else do
                              line <- hGetLine inh
                              let result = eval1 parsedProg out (convert line)
                              if(isMyNumber (snd result))
                                then do
                                  if (read (snd result) == length out)
                                    then do return ()
                                    else do
                                      execute parsedProg result line inh out
                                else do
                                      execute parsedProg result line inh out


--convert one line of the input
convert :: String -> [Int]
convert s = map read (words s)

myprint :: [Int] -> IO()
myprint [] = return ()
myprint (x:xs) = do
                  putStr((show x) ++ " ")
                  myprint xs

isMyNumber :: String -> Bool
isMyNumber [] = False
isMyNumber (x:[]) | isDigit x = True
                  | otherwise = False
isMyNumber (x:xs) | (isDigit x) = isMyNumber xs
                  | otherwise = False

printArray :: [Int] -> IO()
printArray [] = return ()
printArray (x:xs) = do
                      putStr((show x) ++ " ")
                      printArray xs

printAppend :: [Int] -> [Int] -> Int -> Int -> Int -> IO()
printAppend acc (x:xs) numStream index len
                                          | len == numStream = printArray (x:xs)
                                          | index == len - 1 = do
                                                                  putStr("\n")
                                                                  printAppend [] (acc ++ [x]) numStream 0 ((length acc) + 1)
                                          | index `mod` (len `div` numStream) == 0 = do
                                                                                        putStr((show x) ++ " ")
                                                                                        printAppend acc xs numStream (index+1) len
                                          | otherwise = printAppend (acc ++ [x]) xs numStream (index+1) len


execute :: Exp -> ([Int],String) -> String -> Handle -> [[Int]] -> IO()
execute parsedProg result line inh out = do
                              if((snd result) == "append")
                                then do
                                        printAppend [] (fst result) (length (convert line)) 0 (length (fst result))
                                        putStr("\n")
                                        putStr (line)
                                        putStr("\n")
                                        mainloop parsedProg inh ([fst result] ++ out)
                                else do
                                  myprint (fst result)
                                  putStr("\n")
                                  mainloop parsedProg inh ([fst result] ++ out)
