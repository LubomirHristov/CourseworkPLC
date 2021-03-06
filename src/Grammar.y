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
prepend     {TokenPrepend _}
copy        {TokenCopy _}
In          {TokenIn _}
out         {TokenOut _}
sum         {TokenSum _}
'['         {TokenLArr _}
']'         {TokenRArr _}
'('         {TokenLParen _}
')'         {TokenRParen _}
'+'         {TokenPlus _}
'-'         {TokenMinus _}
'*'         {TokenTimes _}
'/'         {TokenDiv _}
','         {TokenComma _}
emptyPrepend {TokenEmptyPrepend _}
num         {TokenNum _ $$}
var         {TokenVar _ $$}

%%


Exp:

     Exp ',' Exp       {MyTokenSeparator $1 $3}
    | Exp Op Exp       {MyTokenExpOperation $1 $2 $3}
    | limit num        {MyTokenLimit  $2}
    | sum Exp          {MyTokenSum $2}
    | In '[' Exp ']'   {MyTokenInArr $3}
    | In               {MyTokenIn}
    | out '[' Exp ']'  {MyTokenOutArr $3}
    | out              {MyTokenOut}
    | var Op Exp       {MyTokenStreamOp $1 $2 $3}
    | var Op           {MyTokenVarOp $1 $2}
    | var              {MyTokenVar $1}
    | num              {MyTokenNum $1}
    | '(' Exp ')'      {$2}

Op:
     prepend '[' Exp ']'  {MyTokenPrepend $3}
    | emptyPrepend      {MyTokenEmptyPrepend}
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

data Op = MyTokenPrepend Exp | MyTokenEmptyPrepend | MyTokenCopy | MyTokenPlusNum Int | MyTokenMinusNum Int | MyTokenTimesNum Int | MyTokenDivNum Int | MyTokenPlus | MyTokenMinus | MyTokenTimes | MyTokenDiv
  deriving (Eq, Show)

data Exp = MyTokenLimit Int | MyTokenSeparator Exp Exp | MyTokenExpOperation Exp Op Exp| MyTokenSum Exp | MyTokenInArr Exp | MyTokenIn | MyTokenOutArr Exp | MyTokenOut | MyTokenStreamOp String Op Exp | MyTokenVarOp String Op | MyTokenVar String | MyTokenNum Int
  deriving (Eq,Show)
}
