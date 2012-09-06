module Return where

-- Return is like Data.Maybe, but the error case carries an error message

data Return r = Ok r
              | Error String
	deriving (Show,Eq)

-- a State Monad that may fail.
-- We cannot reuse Control.Monad.State because that one forgets it's state on failure.

data ReturnState s a = ReturnState { runR :: s -> (s, Return a) }

instance Monad Return where
	return             = Ok 
	(Ok a)      >>= f  = f a
	(Error msg) >>= f  = fail msg 
	fail               = Error

instance Functor Return where
	fmap f (Ok a)      = Ok (f a)
	fmap f (Error msg) = Error msg

instance Monad (ReturnState s) where
	return a = ReturnState $ \state -> (state, Ok a)
	r >>= f  = ReturnState $ \state ->
		case runR r state of
			(state', Ok a)      -> runR (f a) state'
			(state', Error msg) -> (state', Error msg)
	fail msg = ReturnState $ \state -> (state, Error msg)

instance Functor (ReturnState s) where
	fmap f r = do
		a <- r
		return (f a)

fromOk (Ok a) = a

-- transform error message, leaves Ok values untouched
catchR :: Return a -> (String -> Return a) -> ReturnState s a
catchR r handler = ReturnState $ \state ->
	case r of
		(Ok a)      -> (state, Ok a)
		(Error msg) -> (state, handler msg)

-- transform error messages produced by state transformer, leaves Ok values untouched
catchRS :: ReturnState s a -> (String -> Return a) -> ReturnState s a
catchRS rs handler = ReturnState $ \state ->
	case runR rs state of
		(state', Ok a)      -> (state', Ok a)
		(state', Error msg) -> (state', handler msg)

-- catchRS with arguments flipped
handleRS = flip catchRS



