-- main = do
--   some <- getLine
--   if null some
--     then return ()
--     else (do
--       putStrLn $ reverseWords some
--       main)
--
-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words
import Control.Monad

main = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    main
