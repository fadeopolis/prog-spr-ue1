module Stack( Stack, new, push, pop, peek, depth ) where

-- abstract data type for stacks
-- supports the usual stack operations

import Value
import Data.List

data Stack = Stack [Value]

-- create a new empty stack
new :: Stack
new = Stack []

-- add one value to the top of stack
push :: Value -> Stack -> Stack
push v (Stack s) = Stack (v:s)

-- remove one element from the top of stack.
-- an empty stack remains unchanged
pop :: Integer -> Stack -> Stack
pop n (Stack s) = Stack (take i s ++ drop (i+1) s)
	where
		i = fromInteger n

-- retrieves the nth element of the stack, 
-- if n > depth of stack, an exception is thrown
peek :: Integer -> Stack -> Value
peek n (Stack s) = s !! i
	where
		i = fromInteger n

-- returns the number of elements in the stack
depth :: Stack -> Integer
depth (Stack s) = toInteger $ length s

instance Show Stack where
	show (Stack s) = concat $ intersperse " " $ reverse $ map show s

