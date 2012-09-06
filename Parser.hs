module Parser( parse ) where

-- the parser parses an instruction from the input string of a Vm

import Value
import Vm
import Return
import Instruction

import Data.Char

import Test.QuickCheck((==>))

type Parser a = ReturnState Vm a

-- *** top level parsing function

parse :: Parser Instruction
parse = comment >> space >> oneOf [numberLiteral, charLiteral, stringLiteral, operator]

-- *** parse operators and literals

numberLiteral = fmap (literal . Num . Prelude.read) (many1 digit)

stringLiteral = fmap (literal . Str) $ handleRS err $ do
	char '['
	str <- stringBody
	char ']'
	return str
		where
			string :: Parser String
			string = do
				char '['
				str <- stringBody
				char ']'
				return ("[" ++ str ++ "]")
			stringBody :: Parser String
			stringBody = fmap concat (many (oneOf [string, many1 (noneOf "[]")]))

			err "EOF" = Error "EOF in string literal"
			err msg   = Error msg

charLiteral = fmap (literal . Num . toInteger) $ do
	char '\\'
	c <- sat isAscii
	return (ord' c)
		where
			ord' 't' = ord' '\t'
			ord' 'n' = ord' '\n'
			ord' c   = ord c

operator = do
	c <- item
	case c of 
		'+'  -> return opAdd
		'-'  -> return opSub
		'*'  -> return opMul
		'/'  -> return opDiv
		'%'  -> return opMod
		'~'  -> return opNeg
		'&'  -> return opAnd
		'|'  -> return opOr
		'='  -> return opEq
		'<'  -> return opLt
		'>'  -> return opGt
		'!'  -> return opDup
		'#'  -> return opPop
		'@'  -> return opApp
		'"'  -> return opPrintTxt
		'\'' -> return opPrintNum
		'?'  -> return opRead
		'.'  -> return opShow
		':'  -> return opClearStack
		'^'  -> return opClearScreen
		c    -> fail $ "Illegal operator symbol: '" ++ [c] ++ "'"
		
-- *** basic parser combinators

-- backtracking choice, chooses first alternative that matches input
oneOf :: [Parser a] -> Parser a
oneOf []     = fail "No alternative matched"
oneOf [p]    = p
oneOf (p:ps) = ReturnState $ \s ->
	case runR p s of
		(s', Ok a)    -> (s', Ok a)
		(s', Error _) -> runR (oneOf ps) s

p ||| q = oneOf [p,q]

-- succeeds if none of the given characters matches input
noneOf :: String -> Parser Char
noneOf cs = sat (\c -> notElem c cs)

-- read one character
item :: Parser Char
item = Vm.read

-- read one character if it fulfills a predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do
	c <- item
	if p c then
		return c
	else 
		fail "Parse error"

-- read a letter
letter :: Parser Char
letter = sat isLetter

-- read a digit
digit :: Parser Char
digit  = sat isDigit

-- read one character if it matches a given character
char :: Char -> Parser Char
char c = sat (==c)

-- apply parser zero or more times
many :: Parser a -> Parser [a]
many p = oneOf [many1 p, return []]

-- apply parser one or more times
many1 :: Parser a -> Parser [a]
many1 p = do 
	head <- p
	tail <- many p
	return (head:tail)

-- consume whitespace
space :: Parser ()
space = fmap (const ()) (many (sat isSpace))

-- consume comments
comment :: Parser ()
comment = do
	oneOf [many comment', return []]
	return ()
	where	
		comment' = do
			space
			char ';'
			many (noneOf "\n")
			char '\n'
			return ()


