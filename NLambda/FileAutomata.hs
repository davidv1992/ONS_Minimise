module FileAutomata
  ( printAutomata
  , minimizeAutomata
  ) where

import NLambda
import System.Environment
import System.IO

getList 0 = singleton []
getList 1 = NLambda.map (\x -> [x]) atoms
getList n = NLambda.pairsWithFilter (\x l -> maybeIf (x `lt` head l) (x:l)) atoms (getList (n-1))

buildAlphabet [] = empty
buildAlphabet (l:ls) = union (buildAlphabet ls) (NLambda.map (\x -> (idx, x)) (getList supportSize))
                     where line_exp = words l
                           idx = read (line_exp !! 0) :: Int
                           supportSize = read (line_exp !! 1) :: Int

buildStatesAndFinal [] = (empty, empty)
buildStatesAndFinal (l:ls) = (union prevStates newOrbit, nextFinal isFinal)
                             where line_exp = words l
                                   idx = read (line_exp !! 0) :: Int
                                   supportSize = read (line_exp !! 1) :: Int
                                   isFinal = read (line_exp !! 2) :: Int
                                   newOrbit = NLambda.map (\x -> (idx, x)) (getList supportSize)
                                   (prevStates, prevFinal) = buildStatesAndFinal ls
                                   nextFinal 0 = prevFinal
                                   nextFinal 1 = union prevFinal newOrbit

buildDelta [] = empty
buildDelta (l:ls) = union (NLambda.map (\x -> ((sidx, suppS), (aidx, suppA), (tidx, suppT))) (getList (length prodMap))) (buildDelta ls)
  where
    line_exp = words l
    sidx = read (line_exp !! 0) :: Int
    aidx = read (line_exp !! 1) :: Int
    prodMap = line_exp !! 2
    tidx = read (line_exp !! 4) :: Int
    mask = line_exp !! 5
    zipFilterNot x l1 l2 = fmap snd . Prelude.filter (\y -> fst y /= x) $ zip l1 l2
    suppS = zipFilterNot 'B' prodMap x
    suppA = zipFilterNot 'A' prodMap x
    suppT = zipFilterNot '0' mask x

parseAutomaton file = automaton states alphabet delta empty final
                      where content = lines file
                            header = words $ head content
                            nAlph = read (header !! 1) :: Int
                            nStates = read (header !! 2) :: Int
                            (states, final) = buildStatesAndFinal (take nStates $ drop (3 + nAlph) content)
                            alphabet = buildAlphabet (take nAlph $ drop 2 content)
                            delta = buildDelta (drop (4 + nAlph + nStates) content)

printAutomata [] = return ()
printAutomata (f:fs) = do
  withFile f ReadMode (\h -> hGetContents h >>= print . parseAutomaton)
  printAutomata fs

minimizeAutomata [] = return ()
minimizeAutomata (f:fs) = do
  withFile f ReadMode (\h -> hGetContents h >>= print . minimize . parseAutomaton)
  minimizeAutomata fs
