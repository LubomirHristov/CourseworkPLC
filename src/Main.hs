import Tokens
import Grammar
import Eval
import System.Environment
import Control.Exception
import System.IO

main = do
          inh <- openFile "input.txt" ReadMode
          sourceText <- readFile "program5.txt"
          let parsedProg = parseCalc (alexScanTokens sourceText)
          mainloop parsedProg inh []
          hClose inh

mainloop parsedProg inh out = do
                 ineof <- hIsEOF inh
                 if ineof
                        then do
                              return ()
                        else do
                              line <- hGetLine inh
                              let result = eval1 parsedProg out (convert line)
                              if((snd result) == "append")
                                then do
                                        myprint (fst result)
                                        putStr (line)
                                else do
                                  myprint (fst result)
                                  putStr("\n")
                                  mainloop parsedProg inh ([fst result] ++ out)

--convert one line of the input
convert :: String -> [Int]
convert s = map read (words s)

myprint :: [Int] -> IO()
myprint [] = return ()
myprint (x:xs) = do
                  putStr((show x) ++ " ")
                  myprint xs
