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
    "Lmax" -> print . minimize $ lmaxExample
    "Lint" -> print . minimize $ lintExample
    "Fifo" -> print . minimize $ fifoExample (read (head w) :: Int)
    "DoubleWord" -> print . minimize $ runningExample atoms (read (head w) :: Int)
    "File" -> minimizeAutomata w
    "Formula" -> minimizeFormulaAutomaton (head w)
