{-# OPTIONS_GHC -w #-}
module Grammar where
import Tokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,49) ([2336,9228,385,2048,0,8194,16384,6162,0,1536,15,512,33060,9345,48,512,0,0,0,0,0,0,0,32834,12324,0,16384,32,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","Exp","Op","limit","append","copy","out","'['","']'","'('","')'","'+'","'-'","'*'","'/'","','","num","var","%eof"]
        bit_start = st * 21
        bit_end = (st + 1) * 21
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..20]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (6) = happyShift action_3
action_0 (9) = happyShift action_4
action_0 (12) = happyShift action_5
action_0 (19) = happyShift action_6
action_0 (20) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (6) = happyShift action_3
action_1 (9) = happyShift action_4
action_1 (12) = happyShift action_5
action_1 (19) = happyShift action_6
action_1 (20) = happyShift action_7
action_1 (4) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (18) = happyShift action_9
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (19) = happyShift action_19
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (10) = happyShift action_18
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (6) = happyShift action_3
action_5 (9) = happyShift action_4
action_5 (12) = happyShift action_5
action_5 (19) = happyShift action_6
action_5 (20) = happyShift action_7
action_5 (4) = happyGoto action_17
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_7

action_7 (7) = happyShift action_11
action_7 (8) = happyShift action_12
action_7 (14) = happyShift action_13
action_7 (15) = happyShift action_14
action_7 (16) = happyShift action_15
action_7 (17) = happyShift action_16
action_7 (5) = happyGoto action_10
action_7 _ = happyReduce_6

action_8 (18) = happyShift action_9
action_8 (21) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (6) = happyShift action_3
action_9 (9) = happyShift action_4
action_9 (12) = happyShift action_5
action_9 (19) = happyShift action_6
action_9 (20) = happyShift action_7
action_9 (4) = happyGoto action_28
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (6) = happyShift action_3
action_10 (9) = happyShift action_4
action_10 (12) = happyShift action_5
action_10 (19) = happyShift action_6
action_10 (20) = happyShift action_7
action_10 (4) = happyGoto action_27
action_10 _ = happyReduce_5

action_11 (19) = happyShift action_26
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_10

action_13 (19) = happyShift action_25
action_13 _ = happyReduce_15

action_14 (19) = happyShift action_24
action_14 _ = happyReduce_16

action_15 (19) = happyShift action_23
action_15 _ = happyReduce_17

action_16 (19) = happyShift action_22
action_16 _ = happyReduce_18

action_17 (13) = happyShift action_21
action_17 (18) = happyShift action_9
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (6) = happyShift action_3
action_18 (9) = happyShift action_4
action_18 (12) = happyShift action_5
action_18 (19) = happyShift action_6
action_18 (20) = happyShift action_7
action_18 (4) = happyGoto action_20
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_2

action_20 (11) = happyShift action_29
action_20 (18) = happyShift action_9
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_8

action_22 _ = happyReduce_14

action_23 _ = happyReduce_13

action_24 _ = happyReduce_12

action_25 _ = happyReduce_11

action_26 _ = happyReduce_9

action_27 (18) = happyShift action_9
action_27 _ = happyReduce_4

action_28 (18) = happyShift action_9
action_28 _ = happyReduce_1

action_29 _ = happyReduce_3

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (MyTokenSeparator happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyTerminal (TokenNum _ happy_var_2))
	_
	 =  HappyAbsSyn4
		 (MyTokenLimit  happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 4 4 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (MyTokenOutArr happy_var_3
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn4
		 (MyTokenStreamOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  4 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn4
		 (MyTokenVarOp happy_var_1 happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  4 happyReduction_6
happyReduction_6 (HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn4
		 (MyTokenVar happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  4 happyReduction_7
happyReduction_7 (HappyTerminal (TokenNum _ happy_var_1))
	 =  HappyAbsSyn4
		 (MyTokenNum happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  5 happyReduction_9
happyReduction_9 (HappyTerminal (TokenNum _ happy_var_2))
	_
	 =  HappyAbsSyn5
		 (MyTokenAppend happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn5
		 (MyTokenCopy
	)

happyReduce_11 = happySpecReduce_2  5 happyReduction_11
happyReduction_11 (HappyTerminal (TokenNum _ happy_var_2))
	_
	 =  HappyAbsSyn5
		 (MyTokenPlusNum  happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  5 happyReduction_12
happyReduction_12 (HappyTerminal (TokenNum _ happy_var_2))
	_
	 =  HappyAbsSyn5
		 (MyTokenMinusNum happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  5 happyReduction_13
happyReduction_13 (HappyTerminal (TokenNum _ happy_var_2))
	_
	 =  HappyAbsSyn5
		 (MyTokenTimesNum happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  5 happyReduction_14
happyReduction_14 (HappyTerminal (TokenNum _ happy_var_2))
	_
	 =  HappyAbsSyn5
		 (MyTokenDivNum   happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  5 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn5
		 (MyTokenPlus
	)

happyReduce_16 = happySpecReduce_1  5 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn5
		 (MyTokenMinus
	)

happyReduce_17 = happySpecReduce_1  5 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn5
		 (MyTokenTimes
	)

happyReduce_18 = happySpecReduce_1  5 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn5
		 (MyTokenDiv
	)

happyNewToken action sts stk [] =
	action 21 21 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLimit _ -> cont 6;
	TokenAppend _ -> cont 7;
	TokenCopy _ -> cont 8;
	TokenOut _ -> cont 9;
	TokenLArr _ -> cont 10;
	TokenRArr _ -> cont 11;
	TokenLParen _ -> cont 12;
	TokenRParen _ -> cont 13;
	TokenPlus _ -> cont 14;
	TokenMinus _ -> cont 15;
	TokenTimes _ -> cont 16;
	TokenDiv _ -> cont 17;
	TokenComma _ -> cont 18;
	TokenNum _ happy_dollar_dollar -> cont 19;
	TokenVar _ happy_dollar_dollar -> cont 20;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 21 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parseCalc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError [] = error "Unknown parse error"
parseError (t:ts) = error ("Parse error at " ++ (tokenPosn t))

data Op = MyTokenAppend Int | MyTokenCopy | MyTokenPlusNum Int | MyTokenMinusNum Int | MyTokenTimesNum Int | MyTokenDivNum Int | MyTokenPlus | MyTokenMinus | MyTokenTimes | MyTokenDiv
  deriving (Eq, Show)

data Exp = MyTokenLimit Int | MyTokenSeparator Exp Exp | MyTokenOutArr Exp | MyTokenStreamOp String Op Exp | MyTokenVarOp String Op | MyTokenVar String | MyTokenNum Int
  deriving (Eq,Show)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
