--Solution to problem 3.3.1

mid :: [a] -> a
mid (x:y:z) = y

first :: [a] -> a
first (x:y:z) = x

last' :: [a] -> a
last' (x:y:z:_) = z

diagcheck :: [[Char]] -> Char
diagcheck mat 
	| (first one,mid two,last three) == ('o','o','o') = 'o'
	| (first one,mid two,last three) == ('x','x','x') = 'x'
	| otherwise = 's'
	where (one,two,three) = (first mat, mid mat, last mat)

diagcheck2 :: [[Char]] -> Char
diagcheck2 mat 
	| (last one,mid two,first three) == ('o','o','o') = 'o'
	| (last one,mid two,first three) == ('x','x','x') = 'x'
	| otherwise = 's'
	where (one,two,three) = (first mat, mid mat, last mat)

rowcheck :: ([[Char]]->[Char])->[[Char]] -> Char
rowcheck func mat
	| (first row,mid row,last row) == ('o','o','o') = 'o'
	| (first row,mid row,last row) == ('x','x','x') = 'x'
	| otherwise = 's'
	where row = func mat

columncheck ::([Char] -> Char) -> [[Char]] -> Char
columncheck func mat
	| (func one,func two,func three) == ('o','o','o') = 'o'
	| (func one,func two,func three) == ('x','x','x') = 'x'
	| otherwise = 's'
	where (one,two,three) = (first mat, mid mat, last mat)

result :: [[Char]] -> Char
result mat = foldl (\acc x -> if(x/='s') then x else acc) 's' [columncheck first mat, columncheck mid mat, columncheck last' mat, rowcheck first mat, rowcheck last' mat,rowcheck mid mat, diagcheck2 mat, diagcheck mat]

nextMoves :: [[Char]] -> Char -> [[[Char]]]
nextMoves mat this = filter (\x -> if (x /= mat) then if((length $ x) == (length $ mat)) then True else False else False) $ next mat this

next :: [[Char]] -> Char -> [[[Char]]]
next [] _ = [[[]]]
next (x:xs) this = (map (\x -> [x]++xs) (processRow this x))++(map ([x]++) (next xs this))

sameRow :: [Char] -> [[Char]]
sameRow (c) = [c]

processRow :: Char -> [Char] -> [[Char]]
processRow _ [] = [[]]
processRow this (x:xs)
	| x == 's' = (map ([this]++) (sameRow xs)) ++ (map ([x]++) (processRow this xs))
	| otherwise = (map ([x]++) (processRow this xs))

