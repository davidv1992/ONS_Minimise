import NLambda
import System.Environment
import System.IO

getList 0 = singleton []
getList 1 = NLambda.map (\x->[x]) atoms
getList n = NLambda.filter (\(x:y:t)-> lt x y) (NLambda.sum $ NLambda.map (\x -> NLambda.map (\l -> x:l) (getList (n-1))) atoms)

getOrbit n = NLambda.map (\l->unions (Prelude.map (\x->singleton x) l)) (getList n)

buildAlphabet [] = empty
buildAlphabet (l:ls) = union (buildAlphabet ls) (NLambda.map (\x -> (idx, x)) (getOrbit supportSize))
                     where line_exp = words l
                           idx = read (line_exp !! 0) :: Int
                           supportSize = read (line_exp !! 1) :: Int

buildStatesAndFinal [] = (empty, empty)
buildStatesAndFinal (l:ls) = (union prevStates newOrbit, nextFinal isFinal)
                             where line_exp = words l
                                   idx = read (line_exp !! 0) :: Int
                                   supportSize = read (line_exp !! 1) :: Int
                                   isFinal = read (line_exp !! 2) :: Int
                                   newOrbit = (NLambda.map (\x -> (idx, x)) (getOrbit supportSize))
                                   (prevStates, prevFinal) = buildStatesAndFinal ls
                                   nextFinal 0 = prevFinal
                                   nextFinal 1 = union prevFinal newOrbit

buildDelta [] = empty
buildDelta (l:ls) = union (NLambda.map (\x -> ((sidx, filterA prodMap x), (aidx, filterB prodMap x), (tidx, filterMask mask x))) (getList (length prodMap))) (buildDelta ls)
                    where line_exp = words l
                          sidx = read (line_exp !! 0) :: Int
                          aidx = read (line_exp !! 1) :: Int
                          prodMap = line_exp !! 2
                          tidx = read (line_exp !! 4) :: Int
                          mask = line_exp !! 5
                          filterA [] [] = empty
                          filterA ('B':m) (r:rs) = filterA m rs
                          filterA (c:m) (r:rs) = insert r (filterA m rs)
                          filterB [] [] = empty
                          filterB ('A':m) (r:rs) = filterB m rs
                          filterB (c:m) (r:rs) = insert r (filterB m rs)
                          filterMask [] [] = empty
                          filterMask ('0':m) (r:rs) = filterMask m rs
                          filterMask (c:m) (r:rs) = insert r (filterMask m rs)

parseAutomaton file = automaton states alphabet delta empty final
                      where content = lines file
                            header = words $ head content
                            nAlph = read (header !! 1) :: Int
                            nStates = read (header !! 2) :: Int
                            (states, final) = buildStatesAndFinal (take nStates $ drop (3 + nAlph) content)
                            alphabet = buildAlphabet (take nAlph $ drop 2 content)
                            delta = buildDelta (drop (4 + nAlph + nStates) content)

handleFiles [] = return ()
handleFiles (f:fs) = do h <- openFile f ReadMode
                        content <- hGetContents h
                        print $ minimize $ parseAutomaton content
                        handleFiles fs

main = do x <- getArgs
          handleFiles x
