import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageData <- BC.readFile fileName
    glitchedData <- foldM (\bytes func -> func bytes) imageData 
        [randomReplaceByte, randomSortSection, randomReplaceByte, randomSortSection, randomReplaceByte]
    let glitchedFileName = mconcat ["glitched", fileName]
    BC.writeFile glitchedFileName glitchedData

intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc val str = newStr
    where parts = BC.splitAt loc str
          newByte = intToBC val
          newStr = mconcat [fst parts, newByte, (BC.tail.snd) parts]

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte str = do
    let strLen = BC.length str
    loc <- randomRIO (0, strLen - 1)
    val <- randomRIO (0, 255)
    return (replaceByte loc val str)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size str = newStr
    where (before, rest) = BC.splitAt start str
          (section, after) = BC.splitAt size rest
          newStr = mconcat [before, BC.sort section, after]

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection str = do
    let strLen = BC.length str
    start <- randomRIO (0, strLen - 1)
    let size = 25
    return (sortSection start size str)
