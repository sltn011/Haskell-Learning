import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO

toInts :: L.Text -> [Int]
toInts numsTxt = result
    where parsedLines = L.lines numsTxt
          result = map (read.(L.unpack)) parsedLines :: [Int]

main :: IO ()
main = do
    userInput <- LIO.getContents
    let values = toInts userInput
    putStrLn (show (sum values))

-- Нужно компилировать!