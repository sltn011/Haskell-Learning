import qualified Data.Map as Map

data RobotPart = RobotPart
    { name :: String
    , description :: String
    , cost :: Double
    , count :: Int
    } deriving Show

leftArm :: RobotPart
leftArm = RobotPart
    { name = "Left arm"
    , description = "To punch faces"
    , cost = 1000.00
    , count = 3
    }

rightArm :: RobotPart
rightArm = RobotPart
    { name = "Right arm"
    , description = "For handshaking"
    , cost = 1025.00
    , count = 5
    }

robotHead :: RobotPart
robotHead = RobotPart
    { name = "Robot head"
    , description = "Looks crazy"
    , cost = 5092.25
    , count = 2
    }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part =
    mconcat [ "<h2>", partName, "</h2>"
            , "<p><h3>Description: </h3>", partDesc, "</p>"
            , "<p><h3>Cost: </h3>", partCost, "</p>"
            , "<p><h3>Count: </h3>", partCount, "</p>"]
    where partName = name part
          partDesc = description part
          partCost = show (cost part)
          partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where keys = [1, 2, 3]
          parts = [leftArm, rightArm, robotHead]
          keyVals = zip keys parts

insertSnippet :: Maybe RobotPart -> IO ()
insertSnippet part = do
    print part

allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)