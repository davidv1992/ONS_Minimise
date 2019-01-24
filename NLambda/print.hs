import Fifo
import FileAutomata
import FormulaAutomata
import Lmax
import Lint
import RunningExample

import NLambda
import System.Environment
import System.IO

main = do
  f:w <- getArgs
  case f of
    "Lmax" -> print $ lmaxExample
    "Lint" -> print $ lintExample
    "Fifo" -> print $ fifoExample (read (head w) :: Int)
    "DoubleWord" -> print $ runningExample atoms (read (head w) :: Int)
    "File" -> printAutomata w
    "Formula" -> printFormulaAutomaton (head w)
