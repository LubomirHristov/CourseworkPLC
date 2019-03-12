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
  "print"       {\p s -> TokenPrint p}
  "append"      {\p s -> TokenAppend p}
  "duplicate"   {\p s -> TokenDuplicate p}
  \(            {\p s -> TokenLParen p}
  \)            {\p s -> TokenRParen p}
  $digit+       {\p s -> TokenNum p (read s)}
  $alpha [$alpha $digit \_ \’]*   { \p s -> TokenVar p s}


{
data Token =
     TokenPrint AlexPosn
    | TokenAppend AlexPosn
    | TokenDuplicate AlexPosn
    | TokenLParen AlexPosn
    | TokenRParen AlexPosn
    | TokenNum AlexPosn Int
    | TokenVar AlexPosn String
    deriving (Eq,Show)

tokenPosn :: Token -> String
tokenPosn (TokenPrint  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAppend  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDuplicate  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNum  (AlexPn a l c) n) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) v) = show(l) ++ ":" ++ show(c)

}
