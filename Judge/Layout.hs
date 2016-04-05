module Layout where
import Data.Time.Clock

data Error = CompileFail | MemoryExceeded | TimeExceeded | WriteFail | NoError deriving (Eq,Show)
type Rerror = Either Error (Bool,NominalDiffTime,String,Integer)
type Berror = Either Error Bool	