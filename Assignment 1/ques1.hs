take :: [a] -> Integer -> [a]
take [] _ = []
take (x:xs) n 
	| n > 0 = x:(Main.take xs (n-1))
	| n <= 0 = []

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [] (x:xs) = (x:xs)
merge (x:xs) [] = (x:xs)
merge (x:xs) (y:ys)
	| x < y = x:(merge xs (y:ys))
	| otherwise = y:(merge (x:xs) ys)

length :: [a] -> Integer
length [] = 0
length (x:xs) = 1 + (Main.length xs)

drop :: [a] -> Integer -> [a]
drop [] _ = []
drop (x:xs) n
	| n <= 0 = (x:xs)
	| len > n = Main.drop xs (n-1)
	| len <= n = [] 
	where len = Main.length (x:xs) 