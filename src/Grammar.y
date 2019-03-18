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
limit       {TokenLimit _}
append      {TokenAppend _}
copy        {TokenCopy _}
out         {TokenOut _}
'['         {TokenLArr _}
']'         {TokenRArr _}
'('         {TokenLParen _}
')'         {TokenRParen _}
'+'         {TokenPlus _}
'-'         {TokenMinus _}
'*'         {TokenTimes _}
'/'         {TokenDiv _}
','         {TokenComma _}
num         {TokenNum _ $$}
var         {TokenVar _ $$}

%left '+'
%left '-'
%left '*'
%%


Exp:

     Exp ',' Exp       {MyTokenSeparator $1 $3}
    | Exp Op Exp       {MyTokenExpOperation $1 $2 $3}
    | limit num        {MyTokenLimit  $2}
    | out '[' Exp ']'  {MyTokenOutArr $3}
    | var Op Exp       {MyTokenStreamOp $1 $2 $3}
    | var Op           {MyTokenVarOp $1 $2}
    | var              {MyTokenVar $1}
    | num              {MyTokenNum $1}
    | '(' Exp ')'      {$2}

Op:
     append '[' Exp ']'        {MyTokenAppend $3}
    | copy             {MyTokenCopy}
    | '+' num          {MyTokenPlusNum  $2}
    | '-' num          {MyTokenMinusNum $2}
    | '*' num          {MyTokenTimesNum $2}
    | '/' num          {MyTokenDivNum   $2}
    | '+'              {MyTokenPlus}
    | '-'              {MyTokenMinus}
    | '*'              {MyTokenTimes}
    | '/'              {MyTokenDiv}

{
parseError :: [Token] -> a
parseError [] = error "Unknown parse error"
parseError (t:ts) = error ("Parse error at " ++ (tokenPosn t))

data Op = MyTokenAppend Exp | MyTokenCopy | MyTokenPlusNum Int | MyTokenMinusNum Int | MyTokenTimesNum Int | MyTokenDivNum Int | MyTokenPlus | MyTokenMinus | MyTokenTimes | MyTokenDiv
  deriving (Eq, Show)

data Exp = MyTokenLimit Int | MyTokenSeparator Exp Exp | MyTokenExpOperation Exp Op Exp| MyTokenOutArr Exp | MyTokenStreamOp String Op Exp | MyTokenVarOp String Op | MyTokenVar String | MyTokenNum Int
  deriving (Eq,Show)
}
