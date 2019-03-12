import Tokens
import Grammar
import Eval
import System.Environment
import Control.Exception
import System.IO

--
main = do
           input <- readFile "input.txt"
           -- sourceText <- readFile "program.txt"
           -- putStrLn ("Parsing : " ++ sourceText)
           -- let parsedProg = parseCalc (alexScanTokens sourceText)
           -- putStrLn ("Parsed as " ++ (show parsedProg) ++ "\n")
           -- -- putStrLn ("Type Checking : " ++ (show parsedProg) ++ "\n")
           -- -- let typedProg = typeOf [] parsedProg
           -- -- putStrLn ("Type Checking Passed with type " ++ (unparseType typedProg) ++ "\n")
           -- putStrLn("-----------------------------------------")
           -- let result = eval1 (parsedProg) (convert input 0 [])
           return input
          -- putStrLn ("Evaluates to " ++ (unparse result) ++ "\n")

main' :: IO()
main' = do
       inh <- openFile "input.txt" ReadMode
       mainloop [] inh True
       hClose inh

--pass input line by line
mainloop :: [[Int]] -> Handle -> Bool-> IO ()
mainloop acc inh flag =
    do sourceText <- readFile "program2.txt" --read our program
       let parsedProg = parseCalc (alexScanTokens sourceText)
       ineof <- hIsEOF inh
       if ineof
           then do
                  putStrLn(show acc)
                  return ()
           else do inpStr <- hGetLine inh
                   if flag  --if should continue
                      then do 
                              let result = eval1 (parsedProg) (convert' inpStr)
                              mainloop ((fst result) ++ acc) inh (snd result) --call mainloop with the result and a flag that tells us if we should continue making the same operations
                      else do mainloop (acc ++ [(convert' inpStr)]) inh False --concatinate the result wit hthe rest of the input


--convert the whole file
convert :: String -> Int -> [[Int]] -> [[Int]]
convert s index acc
                    | index == (length (lines s)) = acc
                    | otherwise = convert s (index+1) (acc ++ [ints])
                          where
                            arr = lines s
                            ints = map read (words (arr !! index))  :: [Int]

--convert one line of the input
convert' :: String -> [Int]
convert' s = map read (words s)
