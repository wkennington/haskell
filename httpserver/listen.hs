import Network
import Data.Word
import System.IO
import System.Environment
import Control.Concurrent
import qualified Data.Map

getHeader :: [String] -> Handle -> IO [String]
getHeader xs handle = hGetLine handle >>= \s ->
	if s == "" then return xs else getHeader (s:xs) handle

splitFirst :: Char -> String -> (String, String)
splitFirst _ "" = ("","")
splitFirst c (x:s) = if x == c then ("",s) else
	let (k,v) = splitFirst c s in (x:k,v)

parseHeader :: [String] -> Data.Map.Map String String
parseHeader headers = foldl (
	\map s -> let (k,v) = splitFirst ':' s in
	if v == "" then map else Data.Map.insert k v map)
	Data.Map.empty headers

prepareResponse :: Data.Map.Map String String -> [String]
prepareResponse req = 

handler :: (Handle, HostName, PortNumber) -> IO ()
handler (handle, host, port) = hSetNewlineMode handle (NewlineMode CRLF CRLF) >>
	getHeader [] handle >>= \h -> return (parseHeader h) >>= print >> hClose handle

start :: [String] -> IO ()
start [] = start ["8080"]
start [port] = listenOn (PortNumber (fromIntegral (read port::Int))) >>=
	\sock -> mapM_ (\_ -> accept sock >>= \d -> forkIO (handler d)) [1..] >>
		sClose sock

main :: IO ()
main = getArgs >>= start
