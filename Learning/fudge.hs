process :: String -> [String]
process some
  | ((length ins) < 3) = ins
  | otherwise = []
  where
    ins = (words some)

main = do
  putStrLn "running"
