module Instruction where

-- contains code for all builtin instructions for the calculator

import Vm
import Value
import Return

import Data.Char

import Debug.Trace
import System.IO.Unsafe

type Instruction = VmFn ()

opAdd, opSub, opMul, opDiv, opMod, opNeg, opAnd, opOr, opEq, opLt, opGt, opDup, opPop, opApp  :: VmFn ()
opPrintTxt, opPrintNum, opRead, opShow, opClearStack, opClearScreen                           :: VmFn ()

	
-- **** ARITHMETIC "+", "-", "*", "/", "%", "&", "|", "=", "<", ">"

opAdd = mkArithOp "+" (lift2 (+))
opSub = mkArithOp "-" (lift2 (-))
opMul = mkArithOp "*" (lift2 (*))
opDiv = mkArithOp "/" div
	where
		div a 0 = fail "Division by zero"
		div a b = Ok (a `Prelude.div` b)
opMod = mkArithOp "%" mod
	where
		mod a 0 = fail "Division by zero"
		mod a b = Ok (a `Prelude.mod` b)
opAnd = mkLogicOp "&" (&&)
opOr  = mkLogicOp "|" (||)
opEq  = mkBinaryOp eq
	where
		eq a b = Ok $ Num $ toInteger $ fromEnum $ a == b 
opLt  = mkArithOp "<" (boolify (<))
opGt  = mkArithOp "%" (boolify (>))

-- **** NEGATION "~"

opNeg = do
	a <- pop
	case a of
		(Num n) -> push (Num (negate n))
		(Str _) -> fail "Cannot apply ~ to a string"

-- **** COPY "!"

opDup = do
	-- get parameter
	n  <- pop
	n' <- catchR (num n) (\s -> fail $ "cannot apply !, " ++ s)
	-- index check
	check (\vm -> n' > 1)            "(!) Illegal stack index"
	check (\vm -> n' < depth vm + 2) "(!) Stack underflow"
	-- copy
	v  <- peekNth (n' - 2)
	push v

-- **** DELETE "#"

opPop = do
	-- get parameter
	n  <- pop
	n' <- catchR (num n) (\s -> fail $ "cannot apply !, " ++ s)
	-- index check
	check (\vm -> n' > 1)            "(#) Illegal stack index"
	check (\vm -> n' < depth vm + 2) "(#) Stack underflow"
	-- delete
	popNth (n' - 2)
	return ()

-- **** APPLY "@"

opApp = do
	v <- pop
	case v of
		(Num n) -> push (Num n)
		(Str s) -> unread s

-- **** PRINT TEXT """

opPrintTxt = do
	v <- pop
	case v of
		(Num n) -> if isAscii n 
			then write [chr $ fromInteger n] 
			else fail $ "Cannot print " ++ show n ++ " it is not a valid ASCII character"
		(Str s) -> write s
	where
		isAscii c = 0 <= c && c <= 255

-- **** PRINT VALUES "'"

opPrintNum = do
	v <- pop
	write (show v)

-- **** READ STRING "?"

opRead = do
	line <- readline
	pushS line

-- **** PRINT VM STATE "."

opShow = do
	vm <- getVm
	write ("\n" ++ show vm ++ "\n")

-- **** CLEAR STACK ":"

opClearStack = opShow >> clearStack

-- **** CLEAR SCREEN "^"

opClearScreen = write "\ESC[2J\ESC[1;1HScreen cleared" >> nl

-- **** NUMBER OR STRING LITERAL 

literal :: Value -> Instruction
literal value = push value

-- **** HELPERS

mkBinaryOp :: (Value -> Value -> Return Value) -> VmFn ()
mkBinaryOp f = do
	a <- pop
	b <- pop
	case f b a of
		(Ok v)      -> push v
		(Error msg) -> fail msg

mkArithOp :: String -> (Integer -> Integer -> Return Integer) -> VmFn ()
mkArithOp name f = mkBinaryOp f'
	where
		f' (Num a) (Num b) = fmap Num (f a b)
		f' _       _       = fail ("Cannot apply " ++ name ++ " to a string")

mkLogicOp :: String -> (Bool -> Bool -> Bool) -> VmFn ()
mkLogicOp name f = mkBinaryOp f'
	where
		f' (Num 0) (Num 0) = (Ok . Num . toInteger . fromEnum) (f False False)
		f' (Num 0) (Num 1) = (Ok . Num . toInteger . fromEnum) (f False True)
		f' (Num 1) (Num 0) = (Ok . Num . toInteger . fromEnum) (f True  False)
		f' (Num 1) (Num 1) = (Ok . Num . toInteger . fromEnum) (f True  True)
		f' _       _       = fail (name ++ " can only be applied to boolean values")

lift2 f a b = Ok (f a b)

boolify f a b = Ok $ toInteger $ fromEnum $ f a b


