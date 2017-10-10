import NLambda
import Fifo
import System.Environment
import System.IO

main = do [x] <- getArgs
          print $ minimize $ fifoExample ((read x) :: Int)
