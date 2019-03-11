{
  module Tokens where
}

%wrapper "posn"
$digit = 0-9
$alpha = [a-zA-Z]
$eol = [\n]

tokens :-
  "//".*        ;
  $white+       ;
  "input"       {\p s -> TokenInput p}
  "print"       {\p s -> TokenPrint p}
  \:            {\p s -> TokenConcat p}
  $digit+       {\p s -> TokenNum p (read s)}
  $alpha [$alpha $digit \_ \’]*   { \p s -> TokenVar p s}


{
data Token =
     TokenInput AlexPosn
    | TokenPrint AlexPosn
    | TokenConcat AlexPosn
    | TokenNum AlexPosn Int
    | TokenVar AlexPosn String
    deriving (Eq,Show)

tokenPosn :: Token -> String
tokenPosn (TokenInput  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPrint  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenConcat  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNum  (AlexPn a l c) n) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) v) = show(l) ++ ":" ++ show(c)

}
