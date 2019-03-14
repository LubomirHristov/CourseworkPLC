import Tokens
import Grammar
import Eval
import System.Environment
import Control.Exception
import System.IO

--
main = do
          inh <- openFile "input.txt" ReadMode
          sourceText <- readFile "program3.txt"
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
                                  mainloop parsedProg inh (out ++ [fst result])


--pass input line by line
-- mainloop :: [[Int]] -> Handle -> Bool-> IO ()
-- mainloop acc inh flag = return ()
--     -- do sourceText <- readFile "program2.txt" --read our program
--     --    let parsedProg = parseCalc (alexScanTokens sourceText)
--     --    ineof <- hIsEOF inh
--     --    if ineof
--     --        then do
--     --               putStrLn(show acc)
--     --               return ()
--     --        else do inpStr <- hGetLine inh
--     --                if flag  --if should continue
--     --                   then do
--     --                         --  let result = eval1 (parsedProg) (convert' inpStr)
--     --                         --  mainloop ((fst result) ++ acc) inh (snd result) --call mainloop with the result and a flag that tells us if we should continue making the same operations
--     --                   else do mainloop (acc ++ [(convert' inpStr)]) inh False --concatinate the result wit hthe rest of the input



--convert one line of the input
convert :: String -> [Int]
convert s = map read (words s)

myprint :: [Int] -> IO()
myprint [] = return ()
myprint (x:xs) = do
                  putStr((show x) ++ " ")
                  myprint xs
