module Stream(
	Stream,
	empty, open,
	eof,
	read, unread, 
	buffer, clearBuffer
) where

-- a input stream to which you can prepend data
	
import qualified Prelude
import Prelude(
	Monad, Show,
	String, Char, Maybe(..), Bool(..),
	(++))

data Stream = Stream { buffer :: String, input :: String }

-- create an empty stream
empty :: Stream
empty = Stream [] []

-- create a stream with string as content
open :: String -> Stream
open str = Stream [] str

-- read one char from stream,
-- reads from buffer before input is consumed
read :: Stream -> Maybe (Char, Stream)
read (Stream [] [])         = Nothing
read (Stream (c:cs) ds)     = Just (c, Stream cs ds)
read (Stream []     (c:cs)) = Just (c, Stream [] cs)

-- prepend string to stream, 
-- the data is written to the buffer.
unread :: String -> Stream -> Stream
unread str (Stream cs ds) = Stream (str ++ cs) ds

-- check if stream is empty
eof :: Stream -> Bool
eof (Stream [] []) = True
eof _              = False

-- remove all data prepended to streams buffer
clearBuffer :: Stream -> Stream
clearBuffer (Stream _ cs) = Stream [] cs

instance Show Stream where
	show (Stream b i) = "Stream { buffer = '" ++ b ++ "' }"

