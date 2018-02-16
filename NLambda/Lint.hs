{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Lint (lintExample) where

import           GHC.Generics (Generic)
import           NLambda
import           Prelude      (Eq, Int, Maybe (..), Ord, Show, length, reverse,
                               ($), (+), (-), (.), (>=))
import qualified Prelude      ()

data Lint a = Lint_start | Lint_single a | Lint_full a a | Lint_semi a a | Lint_error deriving (Eq, Ord, Show, Generic, NominalType, Contextual)

lintExample ::Automaton (Lint Atom) Atom
lintExample = automaton
    -- states
    (singleton Lint_start
        `union` map Lint_single atoms
        `union` map (\(x,y) -> Lint_semi x y) lttuples
        `union` map (\(x,y) -> Lint_full x y) lttuples
        `union` singleton Lint_error)
    -- alphabet
    atoms
    --transitions
    (map (\a ->(Lint_start, a, Lint_single a)) atoms
        `union` map (\(x,y) -> (Lint_single x, y, Lint_full x y)) lttuples
        `union` map (\(x,y) -> (Lint_single x, y, Lint_error)) getuples
        `union` sum (map (\(x,y) -> mapFilter (\b -> maybeIf ((x `lt` b) /\ (b `lt` y)) (Lint_full x y, b, Lint_semi b y)) atoms) lttuples)
        `union` sum (map (\(x,y) -> mapFilter (\b -> maybeIf ((x `ge` b) \/ (b `ge` y)) (Lint_full x y, b, Lint_error)) atoms) lttuples)
        `union` sum (map (\(x,y) -> mapFilter (\b -> maybeIf ((x `lt` b) /\ (b `lt` y)) (Lint_semi x y, b, Lint_full x b)) atoms) lttuples)
        `union` sum (map (\(x,y) -> mapFilter (\b -> maybeIf ((x `ge` b) \/ (b `ge` y)) (Lint_semi x y, b, Lint_error)) atoms) lttuples)
        `union` map (\a -> (Lint_error, a, Lint_error)) atoms)
    --initial state
    (singleton Lint_start)
    --final states
    (map (\(x,y) -> Lint_full x y) lttuples)
    where
        getuples = sum (map (\a -> mapFilter (\b -> maybeIf (a `ge` b) (a,b)) atoms) atoms)
        lttuples = sum (map (\a -> mapFilter (\b -> maybeIf (a `lt` b) (a,b)) atoms) atoms)
