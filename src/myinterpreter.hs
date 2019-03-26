import Tokens
import Grammar
import Eval
import Types
import System.Environment
import Control.Exception
import System.IO
import Data.Char
import System.IO.Unsafe

main :: IO()
main = do
          (fileName : _) <- getArgs
          sourceText <- readFile fileName
          -- putStrLn ("Parsing : " ++ sourceText)
          let parsedProg = parseCalc (alexScanTokens sourceText)
          -- putStrLn ("Parsed as " ++ (show parsedProg) ++ "\n")
          -- putStrLn ("Type Checking : " ++ (show parsedProg) ++ "\n")
          -- putStrLn ("Type Checking Passed with type " ++ (unparseType typedProg) ++ "\n")
          mainloop parsedProg [] 0

mainloop :: Exp -> [[Int]] -> Int -> IO()
mainloop parsedProg out lineLen = do
                                  ineof <- isEOF
                                  if ineof
                                      then do return()
                                      else do
                                              line <- getLine
                                              case (hasAlpha line) of
                                                True -> error "Input cannot contain alphabetic characters"
                                                False -> return ()
                                              case (lineLen == length(convert line) || lineLen == 0) of
                                                True -> return ()
                                                False -> error "Inconsistent input length"
                                              let lineLen = length (convert line)
                                              let result = eval1 parsedProg out (convert line)
                                              if(isMyNumber (snd result)) --if snd result is the limit
                                                then do
                                                  if (read (snd result) == length out) --if limit is reached
                                                    then do return ()
                                                    else do --else continue reading lines
                                                      execute parsedProg result line out lineLen
                                                else do --else continue reading lines
                                                      execute parsedProg result line out lineLen


hasAlpha :: String -> Bool
hasAlpha [] = False
hasAlpha (s:[]) = not (isDigit s) && not (isSpace s)
hasAlpha (s1:s2:ss)
                | s1 == '-' && (isDigit s2) = hasAlpha ss
                | not (isDigit s1)  && not (isSpace s1) = True
                | otherwise = hasAlpha (s2:ss)

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

--prints the numbers that have to be prepended (handles multiple streams and multiple prepends)
{-
  @parameters
  acc - stores the numbers that don't have to be printed on the current iteration
  (x:xs) - the list to work on
  numStream - the number of streams
  index - current index of the list
  len - original length of the list
-}
printPrepend :: [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> IO()
printPrepend acc (x:xs) numStream index len numOfLines count
                                          | count > numOfLines = return()
                                          | len == numStream = printArray (x:xs)
                                          | index == len - 1 = do
                                                                  putStr("\n")
                                                                  --add the last number to the acc and => increase its length by 1
                                                                  printPrepend [] (acc ++ [x]) numStream 0 ((length acc) + 1) numOfLines (count+1)
                                          | index `mod` (len `div` numStream) == 0 = do
                                                                                        putStr((show x) ++ " ")
                                                                                        --print elem and increase index
                                                                                        printPrepend acc xs numStream (index+1) len numOfLines count
                                          | otherwise = printPrepend (acc ++ [x]) xs numStream (index+1) len numOfLines count --just store the current element

--a helper function that continues evaluating the input
execute :: Exp -> ([Int],String) -> String -> [[Int]] -> Int -> IO()
execute parsedProg result line out lineLen = do
                              if((snd result) == "prepend") --if we have to prepend
                                then do
                                        let list = convertIOlist (getNumberOfLines 0 [])
                                        let numOfLines = head(head list)
                                        let rest = tail list
                                        printPrepend [] (fst result) (length (convert line)) 0 (length (fst result)) numOfLines 0
                                        if numOfLines < (length(fst result) `div` (length (convert line)))
                                                  then do return()
                                                  else do
                                                            putStr("\n")
                                                            putStr (line)
                                                            putStr("\n")

                                                            --putStrLn(show rest)
                                                            printRemainingLines (numOfLines - (length(fst result) `div` (length (convert line)))) rest
                                else do --else just print the simple output
                                  myprint (fst result)
                                  putStr("\n")
                                  mainloop parsedProg ([fst result] ++ out) lineLen

printRemainingLines :: Int -> [[Int]]-> IO()
printRemainingLines i (x:xs)= do
                          if i == 0
                            then do return()
                            else do
                                    printArray x
                                    putStr("\n")
                                    printRemainingLines (i-1) xs


getNumberOfLines :: Int -> [[Int]] -> IO [[Int]]
getNumberOfLines c acc = do
                      ineof <- isEOF
                      if ineof
                        then do
                          return ([[c]] ++ acc)
                        else do
                              line <- getLine
                              getNumberOfLines (c+1) (acc ++ [(convert line)])

convertIOlist :: IO [[Int]] -> [[Int]]
convertIOlist x = unsafePerformIO x
