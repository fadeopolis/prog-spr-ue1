module Main(main) where

import Vm(Vm, new, fromStr, pop, clearBuffer, clearOutput)
import Value
import Return
import Parser

import System.IO
import System.Environment
import Control.Monad

-- *********** PRIME NUMBER TESTING

main = do
	args <- getArgs
	case args of 
		(lo:hi:_) -> test [read lo .. read hi]
		_         -> putStrLn "usage: PrimeTester LO HI"

-- test a list of numbers for primeness and print results
test :: [Int] -> IO ()
test ns = info >> test' 0 ns
	where
		test' i []     = return ()
		test' i (n:ns) = do
			-- run actual prime test
			calc <- runPrimeTest n
			hask <- return (isPrime n)

			-- print results for number n
			putStrLn (show n ++ "\t" ++ show calc ++ "\t" ++ show hask ++ if calc /= hask then "\t!!!!!!" else "")

			-- repeat table header every 50 lines 
			when (i /= 0 && i `mod` 50 == 0) (info >> hFlush stdout)

			-- recur
			test' (i+1) ns

		-- print table header
		info = putStrLn (take 30 (repeat '-')) >> putStrLn (" \tCALC\tHASK") >> putStrLn (take 30 (repeat '-'))

-- haskell version of prime test
isPrime n = loop 2 n
	where
		loop i n | i < n     = (n `mod` i /= 0) && (loop (i+1) n)
		         | otherwise = True

-- calc version of prime test
progIsPrime n = show n ++ " [[[2#2#2# 1] [[1 + 4 ! @] [2#2#2# 0] 5 ! 5 ! % 0 = 2 + # @] 5 ! 5 ! > 2 + # @ ] 3 ! 4 # 2 4 ! @]@"

-- run prime number test program on a number
runPrimeTest :: Int -> IO Bool
runPrimeTest n = 
	let vm = runProgram (progIsPrime n) in
	case runR pop vm of
		(_, Ok (Num 0)) -> return False
		(_, Ok (Num 1)) -> return True
		(_, Error msg)  -> error $ "could not run prime test: " ++msg

-- run program given as a string on a new vm
runProgram :: String -> Vm
runProgram program = fst $ runR run (fromStr program)
	where
		run = handleRS handler (step >> run)

		handler "EOF" = return ()
		handler msg   = fail msg

-- run a single computation step on a vm
step = do
	inst <- parse 
	inst
	return ()

