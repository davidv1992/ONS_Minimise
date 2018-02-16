{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Lmax (lmaxExample) where

import           GHC.Generics (Generic)
import           NLambda
import           Prelude      (Eq, Int, Maybe (..), Ord, Show, length, reverse,
                               ($), (+), (-), (.), (>=))
import qualified Prelude      ()

data Lmax a = Lmax_start | Lmax_single a | Lmax_double a a deriving (Eq, Ord, Show, Generic, NominalType, Contextual)

lmaxExample :: Automaton (Lmax Atom) Atom
lmaxExample = automaton
    -- states
    (singleton Lmax_start
        `union` map Lmax_single atoms
        `union` pairsWith Lmax_double atoms atoms)
    -- alphabet
    atoms
    -- transitions
    (map (\a -> (Lmax_start, a, Lmax_single a)) atoms
        `union` sum (map (\a -> map (\b -> (Lmax_single a, b, Lmax_double a b)) atoms) atoms)
        `union` sum (map (\a -> map (\(b,c) -> (Lmax_double b c, a, Lmax_double b a)) getuples) atoms)
        `union` sum (map (\a -> map (\(b,c) -> (Lmax_double b c, a, Lmax_double c a)) lttuples) atoms))
    -- initial states
    (singleton Lmax_start)
    -- final states
    (map (\a -> Lmax_double a a) atoms)
    where
        getuples = sum (map (\a -> mapFilter (\b -> maybeIf (a `ge` b) (a,b)) atoms) atoms)
        lttuples = sum (map (\a -> mapFilter (\b -> maybeIf (a `lt` b) (a,b)) atoms) atoms)
