import System.Process
import Data.ByteString(hPut)
import Control.Monad (when)
import System.IO
import Data.Time.Clock
import Data.Maybe (isJust)
import Data.Maybe (maybeToList)
import System.Environment (getArgs)
import qualified Data.Text as T
import System.Posix.Resource
import System.Exit
import Layout
import Data.Either (lefts,rights, isLeft)
import Data.List (nub)

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

testThis :: String -> String -> String -> IO (Rerror)
testThis some cas user= do
  setResourceLimit ResourceCPUTime ResourceLimits{hardLimit = ResourceLimit 5, softLimit = ResourceLimit 4}
  (Just hin,Just hout, _,handle) <- createProcess (proc (user++"/"++some++"/"++some) []){std_out =  CreatePipe, std_in = CreatePipe}
  hSetBuffering hin NoBuffering
  inputf <- openFile (some++"/"++cas) ReadMode
  timeN <- getCurrentTime
  putStuff inputf hin
  timeN <- getCurrentTime
  inputAns <- openFile ("ans/"++some++"/"++cas) ReadMode
  hSetBuffering inputAns NoBuffering
  (cool,timeT) <- checkResult hout some inputAns
  timeNN <- getCurrentTime
  code <- getProcessExitCode handle
  if(code /= Nothing) then do
    error<- memTest some cas user
    if(error == MemoryExceeded) then do
      return (Left MemoryExceeded)
    else do
      memory <- processMem user some
      return (Right (cool,(diffUTCTime timeNN (addUTCTime (head $ maybeToList timeT) timeN)),cas,memory))
  else do
    return (Left TimeExceeded)


memTest :: String -> String -> String -> IO (Error)
memTest some cas user = do
  setResourceLimit ResourceStackSize ResourceLimits{hardLimit = ResourceLimit 1048577, softLimit = ResourceLimit 1048577}
  inputf <- readFile (some++"/"++cas)
  (some,_,_) <- readCreateProcessWithExitCode (shell ("valgrind --tool=massif --massif-out-file='"++user++"/"++some++"/mem' " ++ user++"/"++some++"/"++some)) inputf
  if(some == ExitSuccess) then
    return (NoError)
  else do
    return MemoryExceeded

runTest :: String -> [String] -> String -> IO [Rerror]
runTest _ [] _ = do
  return ([])
runTest some (cas:cases) user = do
  x <- testThis some cas user
  list <- runTest some cases user
  return (x:list)

check :: [(Bool,NominalDiffTime,String,Integer)] -> [Error] -> IO (Berror)
check boolList [] = return (Right $ checkRight boolList)
check boolList (x:xs) = return (Left $ checkLeft (x:xs))

checkRight :: [(Bool,NominalDiffTime,String,Integer)] -> Bool
checkRight cases = foldl (\acc (x,_,_,_) ->
  if (x) then True else acc
  ) False cases

checkLeft :: [Error] -> Error
checkLeft errList = head $ nub errList

leftShit :: Either a b -> a
leftShit (Left a) = a

rightShit :: Either a b -> b
rightShit (Right b) = b

testStuff :: [String] -> IO Rerror
testStuff some = do
  (_,Just hout,_,_) <- createProcess (proc "ls" [(last some)]){std_out = CreatePipe}
  cases <- hGetContents hout
  list <- runTest (last some) (lines $ cases) (first some)
  boolean <- (check (rights list) (lefts list))
  if (isLeft boolean) then do
    return (Left $ leftShit boolean)
  else do
    return (Right ((rightShit boolean),(maxTime (rights list)),(first some),(maxMemory (rights list))))

getNumberLeft :: String -> Integer
getNumberLeft (x:some) = read $ some

checkEmpty :: [(T.Text,T.Text)] -> Bool
checkEmpty [] = True
checkEmpty ((x,y):_) = False

first :: [a] -> a
first = foldr1 (\x _ -> x)

maxMemory :: [(Bool,NominalDiffTime,String,Integer)] -> Integer
maxMemory cases = foldl (\acc (_,_,_,x) -> if (x > acc)then x else acc) 0 cases

maxTime :: [(Bool,NominalDiffTime,String,Integer)] -> NominalDiffTime
maxTime cases = foldl (\acc (_,x,_,_) -> if (x > acc)then x else acc) 0 cases

processMem :: String -> String -> IO (Integer)
processMem user ques= do
  buffer <- readFile (user++"/"++ques++"/mem")
  return (maximum $ foldl (\acc [(x,y)] -> let (broken,want) = last $ T.breakOnAll (T.pack "=") y in (getNumberLeft (T.unpack want)):acc) [] $ foldl (\acc x -> if(not $ checkEmpty x) then x:acc else acc) [] $ map (T.breakOnAll (T.pack "mem_heap_B")) (map T.pack $ lines buffer) )

compileCode :: [String] -> IO (Error)
compileCode inpu = do
  (compile,_,_) <- readCreateProcessWithExitCode (shell ("gcc " ++ (first inpu) ++ "/" ++ (last inpu) ++ "/" ++ (last inpu) ++ ".c -o " ++ (first inpu) ++ "/" ++ (last inpu) ++ "/" ++ (last inpu))) ""
  if(same compile ExitSuccess) then return (NoError)
  else return (CompileFail)

lineE :: Rerror -> String
lineE (Left err) = (show)err
lineE (Right (bool,nominalDiffTime,_,integer)) = ((show)bool) ++ "|" ++ ((show)nominalDiffTime) ++ "|" ++ ((show)integer)

writeLog :: [String] -> Rerror -> IO ()
writeLog (user:ques:_) matter = do
  withFile (user++"/log.txt") AppendMode (\handle -> do hPutStrLn handle $ "\n" ++ user ++ "|" ++ (lineE matter) )
  withFile (ques++"-log.txt") AppendMode (\handle -> do hPutStrLn handle $ "\n" ++ user ++ "|" ++ (lineE matter) )


main = do
  inpu <- getArgs
  err <- compileCode inpu
  if(err == CompileFail) then do
    writeLog inpu (Left err)
  else do
    err <- testStuff inpu
    writeLog inpu err
