data Person = Person  { firstName :: String
                      , lastName :: String
                      , age :: Int
                      , height :: Float
                      , phoneNum :: String
                      , flavor :: String
                      } deriving (Show)

data Day  = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
            deriving (Eq, Enum, Show, Read, Bounded, Ord)

type PhoneNum = String
type Name = String
type PhoneBook = [(Name, PhoneNum)]

type AssocList k v = [(k,v)]
type Intmap = Map Int
