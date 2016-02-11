doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmall x = (if x > 100 then x else x*2) + 1

length' xs = sum [1 | _ <- xs]
jlt xxs = [[x | x<-xs, even x]|xs <- xxs]
removeUpperCase :: [Char] -> [Char]
removeUpperCase st = [x | x <-st,x `elem` ['A'..'Z']]
addThree :: Int -> Int -> Int -> Int
addThree x y z = x+y+z
addThrice :: [Int] -> Int
addThrice xs = sum [x | x <- xs]

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number 7!"
lucky x = "Oops! Wrong Answer"

charName :: (Integral a) => a -> String
charName 1 = "One!"
charName 2 = "Two!"
charName 3 = "Three!"
charName 4 = "Four!"
charName 5 = "Five!"
charName x = "Input must be in range of form"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

first :: (a,b,c) -> a
first (a,_,_) = a

tell :: (Show a) => [a] -> String
tell (x:y:_) = show x ++ show y
tell (x:[]) = show x
tell ([]) = "null"

lengthr :: (Num b) => [a] -> b
lengthr [] = 0
lengthr (_:xs) = 1 + (lengthr xs)

capital :: String -> String
capital "" = "String is empty!"
capital all@(x:xs) = "The string " ++  all ++ "starts with " ++ [x]

bmi :: (RealFloat a ) => a -> a -> String
bmi height weight
  | info  <= skinny = "UW"
  | info  <= normal = "Normal"
  | info  <= over = "Overweight"
  | otherwise =  "Whale"
  where info = weight / height ^ 2
        (skinny, normal, over) = (18.5,25,30)

initials :: String -> String -> String
initials a b = [f] ++ ". " ++ [l] ++ ". "
  where (f:_) = a
        (l:_) = b

calcbmi :: (RealFloat a) => [(a,a)] -> [a]
calcbmi xs = [bmi w h | (w,h) <- xs]
  where bmi w h = w / h ^ 2
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideA = 2 * pi * r * h
      topA = pi * r * r
  in sideA + 2*topA

bmifat :: (RealFloat a) => [(a,a)] -> [a]
bmifat xs = [bmi | (w,h)<-xs,let bmi = w/h ^ 2,bmi >=25.5]

stringTest :: String -> String
stringTest xs = "The string is " ++ case xs of  [] -> "Empty"
                                                [x] -> "Singleton!"
                                                xs -> "good to go!"

maximumL :: (Ord a) => [a] -> a
maximumL [] = error "Error"
maximumL [x] = x
maximumL (x:xs) = max x (maximumL xs)

replicate' :: (Ord i, Num i) => i ->a ->[a]
replicate' n x
  | n <= 0    = []
  | otherwise = x:replicate' (n-1) x

cycle' :: (Num a) => [a] -> [a]
cycle [] = error "some"
cycle' xs = xs ++ cycle' xs

reverse' :: String -> String
reverse' (x:xs) = reverse' xs ++ [x]
reverse' [] = []

take' :: (Num a, Ord a) => a->[a]->[a]
take' n _
  | n <= 0 = []
take' n [] = []
take' n (x:xs) = [x] ++ take' (n-1) xs

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let small = quicksort [xt | xt<-xs,xt < x]
      big = quicksort [xt | xt <- xs, xt >= x]
  in small ++ [x] ++ big

flip' :: (a->b->c) ->b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

largestdivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3 == 0
-- let sumodd = sum (takeWhile (<100000) (filter odd (map (^2) [1..])))
-- Collatz Sequences
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n  = n:chain(n `div` 2)
  | odd n   = n:chain(n*3 + 1)
larg t = filter p [100,99..1]
  where p x = ((length (chain x)) >= t)
-- flip' f = \x y -> f y x
sum'' :: (Num a) => [a] -> a
sum'' = foldl (-) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if y==x then True else acc) False ys

flipit :: (a->b->c) -> b->a->c
flipit f = (\x y -> f y x)

maps :: (a->b) ->[a]->[b]
maps f xs = foldr (\x acc -> (f x ): acc) [] xs

maximum2 :: (Ord a) => [a] -> a
maximum2 xs = foldr1 (\x acc -> (max acc x))xs

reverse2 :: String -> String
reverse2 = foldl(\acc x -> x:acc)[]
-- reverse2 some = foldr (\x acc -> acc:x)"" some
check :: (Num a) => [a] -> a
check xs = product $ map (negate. abs) xs

oddSqsum :: (Integral a) => a -> a
oddSqsum a = sum . takeWhile (<a) .filter odd . map (^2) $ [1..]
