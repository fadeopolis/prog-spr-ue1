module Main(main) where

import Vm(Vm, new, fromStr, pop, clearBuffer, clearOutput)
import Value
import Return
import Parser

import System.IO
import System.Environment
import Control.Monad

main = do
	hSetBuffering stdin NoBuffering
	verbose <- isInVerboseMode

	when verbose (putStrLn "Started calculator\n")

	prog <- getContents
	vm'  <- runProgram prog

	when verbose (putStrLn (show vm'))

-- *********** HELPERS 

-- run program given as a string on a new vm
runProgram :: String -> IO Vm
runProgram program = run (fromStr program)
	where
		run vm = do
--			verbose <- isInVerboseMode
--			when verbose (putStrLn ("<" ++ show n ++ "<" ++ show vm ++ ">>"))

			let (vm', r) = step vm
		
			case r of
				(Error "EOF") -> return vm'
				(Ok    out)   -> printOutput out >> run vm'               --(n+1)
				(Error msg)   -> printError  msg >> run (clearBuffer vm') --(n+1)

-- run a single computation step on a vm
step vm = (flip runR) vm $ do
	inst <- parse 
	inst
	out <- clearOutput
	return out

-- check if command line arg -v is given
isInVerboseMode = getArgs >>= return . elem "-v"

-- print string if it is not empty
printOutput [] = return ()
printOutput cs = putStr cs >> hFlush stdout

-- print error message
printError msg = putStrLn ("\nError: " ++ msg)


