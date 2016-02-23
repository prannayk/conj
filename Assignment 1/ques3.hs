sin :: (Ord a,Num a,Enum a, Eq a, Fractional a) => a -> [a]
sin x = zipWith (\x y -> x*y) (cycle [1,-1])  $ zipWith (\x y -> x/y) (map (x^) [1,3..]) (map (\x -> product [1..x]) [1,3..])

sinN :: (Ord a,Num a,Enum a, Eq a, Fractional a) => a -> Int -> [a]
sinN x n = take n $ Main.sin x

absolute :: (Ord a,Num a,Enum a, Eq a, Fractional a) => a -> a -> a
absolute x y
	| l >= -0 = l
	| otherwise = (-1)*l
	where l = y - x

diff :: (Ord a,Num a,Enum a, Eq a, Fractional a) => [a] -> a -> [a]
diff (x:y:xs) epsilon
	| diffe < epsilon = []
	| otherwise = let list = (diff (y:xs) epsilon) in x:list
	where diffe = absolute x y

sinT :: (Ord a,Num a,Enum a, Eq a, Fractional a) => a -> a -> [a]
sinT x epsilon = diff (Main.sin x) epsilon