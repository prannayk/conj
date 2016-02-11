import Data.Map as Map

data LockerState = Taken | Free deriving (Eq, Show)
type Code = String
type LockerMap = Map.Map Int (LockerState , Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup n map =
  case Map.lookup n map of
    Nothing -> Left $ "The locker number " ++ show n ++ " does not exist!"
    Just (state, code) -> if state /= Taken
                            then Right $ code
                            else Left $ "This locker is taken!"

lockers :: LockerMap
lockers = Map.fromList
          [(100,(Taken, "4566"))
          ,(200,(Taken, "4567"))
          ,(30,(Free, "4567"))
          ,(250,(Free, "4567"))
          ,(267,(Taken, "4567"))
          ]
