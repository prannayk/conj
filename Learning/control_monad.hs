import Control.Monad
import Data.Char

main = do
  colors <- forM [1..5] (\a -> do
    putStrLn $ show a ++ "?"
    color <- getLine
    return color)
  mapM_ putStrLn colors
