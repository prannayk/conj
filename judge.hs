import System.Process
import Data.ByteString(hPut)
import Control.Monad (when)
import System.IO
import Data.Time.Clock
import Data.Maybe (isJust)
import Data.Maybe (maybeToList)
import System.Environment (getArgs)

putStuff :: Handle -> Handle -> IO()
putStuff inputf hin = (do
  some <- hIsEOF inputf
  when (not $ some) $ do
    line <- hGetLine inputf
    hPutStrLn hin line
    putStuff inputf hin)

same :: (Eq a) => a -> a -> Bool
same a b = (a==b)

checkResult :: Handle -> String -> Handle -> IO (Bool,Maybe NominalDiffTime)
checkResult hout some inputf= (do
  timeo <- getCurrentTime
  condin <- hIsEOF inputf
  if (not $ condin) then (do
    output <- hGetLine inputf
    cond <- hIsEOF hout
    if (not $ cond) then (do
      output2 <- hGetLine hout
      if (same output2 output) then checkResult hout some inputf else (do
        putStrLn $ "Mismatch : " ++ output ++ " | " ++ output2
        return (False,Nothing))) else return (False, Nothing)) else (do
      times <- getCurrentTime
      return (True,Just (diffUTCTime times timeo))))

testThis :: String -> String -> IO (Bool,NominalDiffTime,String)
testThis some cas = do
  (Just hin,Just hout, _,_ ) <- createProcess (proc "./a.out" []){std_out =  CreatePipe, std_in = CreatePipe}
  hSetBuffering hin NoBuffering
  inputf <- openFile (some++"/"++cas) ReadMode
  timeN <- getCurrentTime
  putStuff inputf hin
  timeN <- getCurrentTime
  inputAns <- openFile ("ans/"++some++"/"++cas) ReadMode
  hSetBuffering inputAns NoBuffering
  (cool,timeT) <- checkResult hout some inputAns
  timeNN <- getCurrentTime
  return (cool,(diffUTCTime timeNN (addUTCTime (head $ maybeToList timeT) timeN)),cas)

runTest :: String -> [String] -> IO [(Bool,NominalDiffTime,String)]
runTest some [] = do
  return ([])
runTest some (cas:cases) = do
  x <- testThis some cas
  list <- runTest some cases
  return (x:list)


check :: [(Bool,NominalDiffTime,String)] -> Bool
check cases = foldl (\acc (x,_,_) ->
  if (not $ x) then False else acc
  ) True cases


testStuff :: String -> IO Bool
testStuff some = do
  (_,Just hout,_,_) <- createProcess (proc "ls" [some]){std_out = CreatePipe}
  cases <- hGetContents hout
  list <- runTest some (lines $ cases)
  return (check list)

main = do
  inpu <- getArgs
  some <- testStuff $ last $ inpu
  if (same True some) then do
    putStrLn $ (show) some
  else
    putStrLn "False"
