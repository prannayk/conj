import Data.Char

main = do
  appendFile "one.txt" ("c\n")
  interact $ unlines. filter ((<10). length). lines
