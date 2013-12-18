module HelloWorld where

helloWorld = print "Hello World"

helloYou = do
        print "What's your name?"
        name <- getLine
        print ("Hi there, " ++ name ++ "!")