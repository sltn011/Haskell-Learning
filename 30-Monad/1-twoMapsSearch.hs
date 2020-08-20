import qualified Data.Map as Map

type UserName = String
type GamerID = Int
type PlayerCredits = Int

usernameDB :: Map.Map GamerID UserName
usernameDB = Map.fromList [(1, "AAA"), (2, "BBB"), (3, "CCC")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("AAA", 1000), ("BBB", 2000), ("CCC", 3000)]

lookupUserName :: GamerID -> Maybe UserName
lookupUserName id = Map.lookup id usernameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits uname = Map.lookup uname creditsDB

gamerIdToCredits :: GamerID -> Maybe PlayerCredits
gamerIdToCredits id = lookupUserName id >>= lookupCredits