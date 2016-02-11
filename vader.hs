import Control.Concurrent
import System.Process
import Data.Maybe
import System.IO (hGetContents)

compete :: [IO a] -> IO a
compete list = do
  mvar <- newEmptyMVar
  tids <- mapM (\action -> forkIO $ action >>= putMVar mvar) list
  result <- takeMVar mvar
  mapM_ killThread tids
  return result

timeout :: IO a -> Int -> IO (Maybe a)
timeout process wsec = compete [fmap Just process, threadDelay wsec >> return Nothing]

run :: String -> IO ()
run ques = do
  (_,Just hout,_,_) <- createProcess(proc "./judge" [ques]){std_out = CreatePipe}
  some <- hGetContents hout
  putStrLn (last . lines $ some)

main = do
  some <- timeout (run "ques1") 3000000
  return ()
