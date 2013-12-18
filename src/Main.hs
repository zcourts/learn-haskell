module Main where
import HelloWorld

main :: IO ()
main = do
        print "Type the name of the example you want to interact with"
        name <- getLine
        runExample name
        
        
runExample "hello-world" = helloWorld
runExample "hello-you" = helloYou
runExample "q" = print "Exiting"
runExample _ = main