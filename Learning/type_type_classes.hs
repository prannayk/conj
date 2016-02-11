import qualified Data.Map as Map

infixl 5 :-:
data List a = Empty | a :-: List a deriving (Show, Eq,Read,Ord)
infixr 5 .++
(.++) :: List [a] -> List [a] -> List [a]
Empty (.++) ys = ys
(x :-: xs) (.++) ys = x :-: (xs (.++) ys)
