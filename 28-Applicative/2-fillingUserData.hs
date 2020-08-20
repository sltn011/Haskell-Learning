data User = User
    { name :: String
    , gamerId :: Int
    , score :: Int
    } deriving Show

maybeName :: Maybe String
maybeName = Just "Slava"

maybeId :: Maybe Int
maybeId = Just 1163

maybeScore1 :: Maybe Int
maybeScore1 = Just 2000

maybeScore2 :: Maybe Int
maybeScore2 = Nothing

maybeUser1 :: Maybe User
maybeUser1 = User <$> maybeName <*> maybeId <*> maybeScore1

maybeUser2 :: Maybe User
maybeUser2 = User <$> maybeName <*> maybeId <*> maybeScore2

readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
    putStrLn "Input user name, id and score:"
    user <- User <$> getLine <*> readInt <*> readInt
    print user