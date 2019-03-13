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
apply       {TokenApply _}
copy        {TokenCopy _}
'('         {TokenLParen _}
')'         {TokenRParen _}
'+'         {TokenPlus _}
'-'         {TokenMinus _}
'*'         {TokenTimes _}
'/'         {TokenDiv _}
','         {TokenComma _}
num         {TokenNum _ $$}

%%


Exp:
    print Exp          {MyTokenPrint  $2}
    | Exp ',' Exp      {MyTokenSeparator $1 $3}
    | apply Op         {MyTokenApply $2}
    | copy             {MyTokenCopy}
    | '(' Exp ')'      {$2}

Op:
    append num         {MyTokenAppend $2}
    | '+' num          {MyTokenPlus  $2}
    | '-' num          {MyTokenMinus $2}
    | '*' num          {MyTokenTimes $2}
    | '/' num          {MyTokenDiv   $2}

{
parseError :: [Token] -> a
parseError [] = error "Unknown parse error"
parseError (t:ts) = error ("Parse error at " ++ (tokenPosn t))

data Op = MyTokenAppend Int | MyTokenPlus Int | MyTokenMinus Int | MyTokenTimes Int | MyTokenDiv Int
  deriving (Eq, Show)

data Exp = MyTokenPrint Exp | MyTokenSeparator Exp Exp | MyTokenApply Op | MyTokenCopy
  deriving (Eq,Show)
}
