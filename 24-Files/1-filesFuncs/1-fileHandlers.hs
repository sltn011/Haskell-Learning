import System.IO

main1 :: IO ()
main1 = do
    myInputFile <- openFile "./inputFile.txt" ReadMode
    firstLine <- hGetLine myInputFile
    secondLine <- hGetLine myInputFile
    putStrLn firstLine
    myOutputFile <- openFile "./outputFile.txt" WriteMode
    hPutStrLn myOutputFile secondLine
    mapM_ hClose [myInputFile, myOutputFile]

main2 :: IO ()
main2 = do
    myInputFile <- openFile "inputFile.txt" ReadMode
    isEmpty <- hIsEOF myInputFile
    if isEmpty
        then error "Empty file"
        else do
            firstLine <- hGetLine myInputFile
            putStrLn firstLine