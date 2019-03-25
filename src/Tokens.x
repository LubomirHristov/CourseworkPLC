{
  module Tokens where
}

%wrapper "posn"
$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  "//".*        ;
  $white+       ;
  "limit"       {\p s -> TokenLimit p}
  "prepend"     {\p s -> TokenPrepend p}
  "copy"        {\p s -> TokenCopy p}
  "out"         {\p s -> TokenOut p}
  \[            {\p s -> TokenLArr p}
  \]            {\p s -> TokenRArr p}
  \(            {\p s -> TokenLParen p}
  \)            {\p s -> TokenRParen p}
  \+            {\p s -> TokenPlus p}
  \-            {\p s -> TokenMinus p}
  \*            {\p s -> TokenTimes p}
  \/            {\p s -> TokenDiv p}
  \,            {\p s -> TokenComma p}
  $digit+       {\p s -> TokenNum p (read s)}
  $alpha [$alpha $digit \_ \’]*   { \p s -> TokenVar p s}


{
data Token =
     TokenLimit AlexPosn
    | TokenPrepend AlexPosn
    | TokenCopy AlexPosn
    | TokenOut AlexPosn
    | TokenLArr AlexPosn
    | TokenRArr AlexPosn
    | TokenLParen AlexPosn
    | TokenRParen AlexPosn
    | TokenPlus AlexPosn
    | TokenMinus AlexPosn
    | TokenTimes AlexPosn
    | TokenDiv AlexPosn
    | TokenComma AlexPosn
    | TokenNum AlexPosn Int
    | TokenVar AlexPosn String
    deriving (Eq,Show)

tokenPosn :: Token -> String
tokenPosn (TokenLimit  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPrepend  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCopy  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTimes  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDiv  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNum  (AlexPn a l c) n) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) v) = show(l) ++ ":" ++ show(c)

}
