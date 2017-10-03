import NLambda
import Fifo
import Set
import RunningExample
import System.Environment
import System.IO

testFifo 0 = do print "Done fifo."
testFifo n = do print $ minimize $ fifoExample (n-1)
                testFifo (n-1)

testRunning 0 = do print "Done running."
testRunning 1 = do print "Done running."
testRunning n = do print $ minimize $ runningExample atoms (n-1)
                   testRunning (n-1)

main = do [x, y, z] <- getArgs
          testFifo ((read x) :: Int)
          testRunning ((read z) :: Int)
