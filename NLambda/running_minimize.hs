import NLambda
import RunningExample
import System.Environment
import System.IO

main = do [x] <- getArgs
          print $ minimize $ runningExample atoms ((read x) :: Int)
