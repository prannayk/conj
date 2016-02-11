import qualified Data.List as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . M.nub

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = let   big = sort [a | a<-xs,a>=x]
                    small = sort [a | a<-xs,a<x]
                    in small ++ [x] ++ big
break' :: (Eq a) => [a] ->[[a]]
break' [] = []
break' (x:xs) = let (fw,rest) = break (/=x) (x:xs) in fw:(break' rest)
