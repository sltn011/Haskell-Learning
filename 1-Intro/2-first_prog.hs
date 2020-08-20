messyMain::IO()
messyMain = do
    putStrLn("Who is recipient?")
    recipient <- getLine
    putStrLn("Book Title")
    title <- getLine
    putStrLn("Who is author?")
    author <- getLine
    putStrLn("Dear " ++ recipient ++ "!\n"
        ++ "Thanks for buying \"" ++ title ++ "\"!\n"
        ++ "With thanks, " ++ author)

main::IO()
main = do
    putStrLn("Who is recipient?")
    recipient <- getLine
    putStrLn("Book Title")
    title <- getLine
    putStrLn("Who is author?")
    author <- getLine
    putStrLn(createEmail recipient title author)


toPart recipient =
    "Dear " ++ recipient ++ "!\n"

bodyPart title =
    "Thanks for buying \"" ++ title ++ "\"!\n"

fromPart author =
    "With thanks, " ++ author

createEmail recipient title author =
    toPart recipient ++
    bodyPart title ++
    fromPart author