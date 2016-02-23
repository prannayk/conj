import System.IO
import System.Environment(getArgs)
import System.Process

main = do
	input <- getArgs
	(_,stdout,_) <- readCreateProcessWithExitCode ((proc "Judge/judge" input ) {cwd = Just "Judge/"}) ""
	putStrLn $ last $ lines stdout
