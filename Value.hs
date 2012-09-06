module Value where

import Return

import GHC.Exts( IsString(..) )

data Value = Num Integer
           | Str String
	deriving Eq

num :: Value -> Return Integer
num (Num n) = return n
num v       = fail (show v ++ " is not a number")

str :: Value -> Return String
str (Str s) = return s
str v       = fail (show v ++ " is not a string")

bool :: Value -> Return Bool
bool (Num 0) = return False
bool (Num 1) = return True
bool v       = fail (show v ++ " is not a boolean")

instance Show Value where
	show (Num n) = show n
	show (Str s) = "[" ++ s ++ "]"

-- these are only here to make typing in the terminal easier
instance IsString Value where
    fromString = Str
instance Num Value where
	fromInteger       = Num . fromInteger
	(Num a) + (Num b) = Num (a + b)
	_       + _       = error "+ not supported"
	(Num a) * (Num b) = Num (a * b)
	_       * _       = error "* not supported"
	abs (Num n)       = Num (abs n)
	abs _             = error "abs not supported"
	signum (Num n)    = Num (signum n)
	signum _          = error "signum not supported"
	negate (Num n)    = Num (negate n)
	negate _          = error "negate not supported"	

