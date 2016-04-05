import System.IO
import System.Environment(getArgs)
import System.Process
import qualified Data.ByteString.Char8 as Byte
import Control.Concurrent
import System.Exit
import System.Directory(removeFile) 

first :: [a] -> a
first (x:xs) = x

give :: Byte.ByteString -> [[String]]
give file = map (map (Byte.unpack)) $ fmap (Byte.split '|') $ (Byte.split '\n' file)

mapping :: [[String]] -> IO (Maybe a)
mapping (x:xs) = do
	--forkIO $ do
		list <- readCreateProcessWithExitCode ((proc "Judge/judge" (x) ) {cwd = Just "Judge/"}) ""
		some <- mapping xs
		return Nothing
mapping [] = do return Nothing

main = do
	input <- Byte.readFile "request.txt"
	mapping $ give input
	removeFile "request.txt"
	return ()
