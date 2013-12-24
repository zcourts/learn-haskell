module ConduitExamples where
import Data.Conduit
import qualified Data.Conduit.List as CL

source :: Int -> Source IO Int -- produces a stream of Ints
source n= CL.sourceList [1..n]

sink :: Sink String IO () -- consumes a stream of Strings, no result
sink = CL.mapM_ putStrLn


conduit :: Conduit Int IO String -- converts Ints into Strings
conduit = CL.map show

main = do
        total <- getLine
        let n = read total::Int
        source n $$ conduit =$ sink
        --alt, with same meaning
        --source $= conduit $$ sink