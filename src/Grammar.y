-- $input
--
-- [@line EOL @line EOL ....]
--
-- @line {$1 " " $3 " " $4 TokenEOL}

{
module Grammar where
import Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }
%token
print       {TokenPrint _}
append      {TokenAppend _}
duplicate   {TokenDuplicate _}
'('         {TokenLParen _}
')'         {TokenRParen _}
num         {TokenNum _ $$}

%%


Exp:
    print Exp          {MyTokenPrint  $2}
    | append num Exp   {MyTokenAppend $2 $3}
    | append num       {MyFinalTokenAppend $2}
    | duplicate        {MyTokenDuplicate}
    | '(' Exp ')'      {$2}
{
parseError :: [Token] -> a
parseError [] = error "Unknown parse error"
parseError (t:ts) = error ("Parse error at " ++ (tokenPosn t))

data Exp = MyTokenPrint Exp | MyTokenAppend Int Exp | MyFinalTokenAppend Int | MyTokenDuplicate
  deriving (Eq,Show)
}
