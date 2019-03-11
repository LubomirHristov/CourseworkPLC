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
input       {TokenInput _}
print       {TokenPrint _}
':'         {TokenConcat _}
num         {TokenNum _ $$}

%%


Exp:
    print num ':' {TokenPrintconcat $2}
{
parseError :: [Token] -> a
parseError [] = error "Unknown parse error"
parseError (t:ts) = error ("Parse error at " ++ (tokenPosn t))

data Exp = TokenPrintconcat Int
  deriving (Eq,Show)
}
