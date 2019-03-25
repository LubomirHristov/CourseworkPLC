module Types where
import Grammar
import Data.Char

data MyType = TyInt | TyArr | TyArrOfArr
  deriving (Show,Eq)

typeOf :: Exp -> MyType
typeOf (MyTokenNum n) = TyInt
typeOf (MyTokenVar s) = TyInt
typeOf (MyTokenLimit n) = TyInt
typeOf (MyTokenVarOp v (MyTokenCopy)) | hasVarType v = TyArrOfArr
typeOf (MyTokenVarOp v op) | hasVarType v && (isValidOp op) = TyArr
typeOf (MyTokenSeparator e1 e2) | typeOf e1 == TyInt && typeOf e2 == TyInt = TyArr
                                | typeOf e1 == TyInt && typeOf e2 == TyArr = TyArr --added case for prepend
                                | typeOf e1 == TyArr && (typeOf e2 == TyArr || typeOf e2 == TyArrOfArr) = TyArrOfArr
typeOf (MyTokenStreamOp v op e) | hasVarType v && (isValidOp op) && typeOf e == TyArr = TyArr
typeOf (MyTokenOutArr e) = TyArr
typeOf (MyTokenExpOperation e1 op e2) | typeOf e1 == TyArr && typeOf e2 == TyArr && (isValidOp op) = TyArr
typeOf _ = error "Invalid type"

isValidOp :: Op -> Bool
isValidOp (MyTokenCopy) = True
isValidOp (MyTokenPlus) = True
isValidOp (MyTokenMinus) = True
isValidOp (MyTokenTimes) = True
isValidOp (MyTokenDiv) = True
isValidOp (MyTokenPlusNum n) | typeOf (MyTokenNum n) == TyInt = True
isValidOp (MyTokenMinusNum n) | typeOf (MyTokenNum n) == TyInt = True
isValidOp (MyTokenTimesNum n) | typeOf (MyTokenNum n) == TyInt = True
isValidOp (MyTokenDivNum n) | typeOf (MyTokenNum n) == TyInt = True
isValidOp (MyTokenPrepend e) | typeOf e == TyArr || typeOf e == TyInt = True
isValidOp _ = error "Not a valid operation"

hasVarType :: String -> Bool
hasVarType (x:xs) = isAlpha x && isTyNumber xs

isTyNumber :: String -> Bool
isTyNumber [] = False
isTyNumber (x:[]) = isDigit x
isTyNumber (x:xs) = isDigit x && isTyNumber xs

unparseType :: MyType -> String
unparseType (TyInt) = "Int"
unparseType (TyArr) = "[Int]"
unparseType (TyArrOfArr) = "[[Int]]"
