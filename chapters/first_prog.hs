messyMain :: IO ()
messyMain = do
    print "who is this email for?"
    recipient <- getLine
    print "what is the title?"
    title <- getLine
    print "who is the author?"
    author <- getLine
    print ( createEmail recipient title author) 

toPart recipient = "Dear " ++ recipient ++ ",\n"
bodyPart bookTitle = "thanks for buying " ++ bookTitle ++ ".\n"
fromPart author = "thanks,\n" ++ author

createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
                                         fromPart author
