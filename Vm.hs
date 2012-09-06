module Vm (
	Vm(), 
	VmFn,
	-- executing vm fns
	getVm, setVm,
	catch,
	-- ctors
	Vm.new, news, fromStr,
	-- I/O
	Vm.read, readline, unread,
	write, nl,
	clearOutput, clearBuffer,
	eof,
	-- stack
	depth,
	push, pushI, pushS,
	pop, popNth,
	peek, peekNth,
	dup, dupNth,
	clearStack,
	-- value
	num, str, bool,
	-- util
	check
) where

-- this file contains the central data structure for the calculator, the Vm.

import Value
import Return

import qualified Stream
import qualified Stack
import Stream(Stream)
import Stack(Stack)

import Debug.Trace

-- a Vm object combines an input stream, an output buffer and a stack.

data Vm = Vm { 
	input  :: Stream, 
	output :: String, 
	stack  :: Stack 
}

-- a VmFn is a function that takes a Vm and returns an altered Vm and a return value,
-- a VmFn may fail, in that case it returns an Err object

type VmFn a = ReturnState Vm a

-- *** CONSTRUCTORS

-- create a new empty Vm
new :: Vm
new = (Vm Stream.empty [] Stack.new)

-- create a new Vm with a given stack and empty input & output 
news :: [Value] -> Vm
news vs = Vm Stream.empty [] (foldr Stack.push Stack.new vs)

-- create a new Vm with a given input and empty stack & output
fromStr :: String -> Vm
fromStr i = new { input = Stream.open i }

-- *** INPUT MANIPULATION

-- read one char from the input 
read :: VmFn Char
read = ReturnState $ \vm -> 
	let s = input vm in
	case Stream.read s of
		Just (c, s') -> (set_input vm s', Ok c)
		Nothing      -> (set_input vm s,  Error "EOF")
	
-- read a line from the input
readline = catchRS (do {
		c <- Vm.read;
		if c == '\n'
		then
			return []
		else do
			cs <- readline
			return (c:cs);
	}) (\s -> Ok [])

-- prepend a string to the input buffer of the Vm
unread :: String -> VmFn ()
unread str = update input set_input (Stream.unread str)

-- write to the output buffer of the Vm
write :: String -> VmFn ()
write str = update output set_output (++str)

-- write a newline to the output buffer
nl :: VmFn ()
nl = write "\n"

-- discards all unread buffered data in the input buffer
clearBuffer :: Vm -> Vm
clearBuffer vm = vm { input = Stream.clearBuffer (input vm) }

-- empties the Vm's output buffer and returns it as a string
clearOutput :: VmFn String
clearOutput = do
	out <- get output 
	update output set_output (const [])
	return out

-- checks if the input buffer is empty
eof :: Vm -> Bool
eof vm = Stream.eof $ input vm

-- *** STACK MANIPULATION

-- removes all elements from the stack
clearStack :: VmFn ()
clearStack = update stack set_stack (const Stack.new)

-- push a value on the stack
push :: Value -> VmFn ()
push v = update stack set_stack (\s -> Stack.push v s)

-- push an integer on the stack
pushI :: Integer -> VmFn ()
pushI i = push (Num i)

-- push a string on the stack
pushS :: String -> VmFn ()
pushS s = push (Str s)

-- pop a value from the stack
pop :: VmFn Value
pop = popNth 0

-- pop the nth value from the stack
popNth :: Integer -> VmFn Value
popNth n = do
	v <- peekNth n
	update stack set_stack (\s -> Stack.pop n s)
	return v

-- return the stacks topmost element without removing it
peek :: VmFn Value
peek = peekNth 0

-- return the stacks nth element without removing it
peekNth :: Integer -> VmFn Value
peekNth n = do
	check (\vm -> depth vm >= n) "Stack underflow"
	get   (\vm -> Stack.peek n (stack vm))

-- copy the stacks topmost element and put it on top of stack
dup :: VmFn ()
dup = dupNth 0

-- copy the stacks nth element and put it on top of stack
dupNth :: Integer -> VmFn ()
dupNth n = do
	v <- peekNth n
	push v

-- **** UTILITY FUNCTIONS

-- like Control.Monad.guard, but an error message is reported in case of failure
check :: (Vm -> Bool) -> String -> VmFn ()
check p msg = do
	vm <- getVm
	if p vm
	then
		return ()
	else
		fail msg

-- get the stacks depth
depth :: Vm -> Integer
depth vm = Stack.depth $ stack vm

-- get current state of vm in stateful computation
getVm :: VmFn Vm
getVm = ReturnState $ \vm -> (vm,Ok vm)

-- set current state of vm in stateful computation
setVm :: Vm -> VmFn ()
setVm vm = ReturnState $ \_ -> (vm,Ok ())

-- get a vm field
get :: (Vm -> a) -> VmFn a
get f = getVm >>= return . f

set_stack :: Vm -> Stack -> Vm
set_stack vm s = vm { stack = s }

set_input :: Vm -> Stream -> Vm
set_input vm i = vm { input = i }

set_output :: Vm -> String -> Vm
set_output vm o = vm { output = o }

update :: (Vm -> a) -> (Vm -> a -> Vm) -> (a -> a) -> VmFn ()
update get set f = do
	vm <- getVm
	let s   = get vm
	let s'  = f s
	let vm' = set vm s'
	setVm vm'

instance Show Vm where
	show (Vm input output stack) = "{" ++ show stack ++ " ^ " ++ Stream.buffer input ++ "}"



