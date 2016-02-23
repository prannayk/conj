zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith func [] _ = []
zipWith func _ [] = []
zipWith func (x:xs) (y:ys) = (func x y):(Main.zipWith func xs ys)

map :: (a->b) -> [a] -> [b]
map func input = foldr (\x acc -> (func x):acc) [] input

foldl :: (a->b->b) -> [a] -> b -> b
foldl func [] y = y
foldl func (x:xs) y = Main.foldl func xs $ (func x y)