import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

getCounts :: T.Text -> (Int, Int, Int)
getCounts input = (numChars, numWords, numLines)
    where numChars = T.length input
          numWords = (length.(T.words)) input
          numLines = (length.(T.lines)) input
          
reportFileCounts :: FilePath -> (Int, Int, Int) -> T.Text
reportFileCounts file (numChars, numWords, numLines) =
    T.pack (mconcat [file, " - ", show numChars, " characters, ", show numWords, " words, ", show numLines, " lines\n"])

main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    fileContent <- TIO.readFile filePath
    let fileCounts = getCounts fileContent
    let fileReport = reportFileCounts filePath fileCounts
    TIO.putStrLn fileReport
    TIO.appendFile "stats.dat" fileReport
