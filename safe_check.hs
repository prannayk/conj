import System.Posix.Resource
import System.Process
import System.Environment(getArgs)
import System.IO

data_in :: String -> IO String
data_in some = do return some

spawn :: IO (Handle)
spawn = do
  (_,_,Just hout,_) <- createProcess (proc "./a.out" []){std_out = CreatePipe}
  return (hout)
roll :: String -> Integer -> IO ()
roll some n = (do
  putStrLn $ some ++ (show n)
  roll some $ n+1
  hout <- spawn
  putStrLn "hahaha")

main = do
  inpu <- getArgs
  setResourceLimit ResourceCPUTime ResourceLimits{hardLimit = ResourceLimit 5, softLimit = ResourceLimit 2}
  putStrLn $ last $ inpu
  setResourceLimit ResourceTotalMemory ResourceLimits{hardLimit = ResourceLimit ((read . last $ inpu) + 1), softLimit = ResourceLimit (read . last $ inpu)}
  -- (_,_,Just hout,_) <- createProcess (proc "./a.out" []){std_out = CreatePipe}
  roll "some" 1
