{-# language RecordWildCards #-}

module FormulaAutomata
  ( minimizeFormulaAutomaton
  , printFormulaAutomaton
  ) where

import qualified Data.Map as Map
import Data.Map ((!))
import Data.Void (Void)
import Control.Monad.Combinators.Expr
import Text.Megaparsec as P
import Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import System.Environment (getArgs)

import NLambda as NL
import Prelude hiding (not, map)


type Var = (Bool, Int) -- state/input + index
type Loc = Int
type Label = Int
type Dimension = Int

data Form
  = Lit Char Var Var -- '<', '=', '>'
  | And Form Form
  | Or Form Form
  | Not Form
  deriving Show

data FAutomatonDescr = FAutomaton
  { alphSize   :: Int
  , statesSize :: Int
  , alph       :: [(Label, Dimension)]
  , locations  :: [(Loc, Dimension, Bool)]
  , trans      :: [(Loc, Label, Form, Loc, [Var])]
  } deriving Show


type Parser = Parsec Void String

-- space consumer and lexer
sc = L.space space1 P.empty P.empty
lexeme = L.lexeme sc
symbol = L.symbol sc

-- some basic parsers
parens = between (symbol "(") (symbol ")")
arrow = symbol "->"
integer = lexeme L.decimal
boolean = lexeme binDigitChar

var :: Parser Var
var = lexeme (toV <$> lowerChar <*> many digitChar) where
  toV 'x' str = (False, read str) -- state var
  toV 'y' str = (True, read str)  -- input var

alphP :: Parser (Label, Dimension)
alphP = (,) <$> integer <*> integer

stateP :: Parser (Loc, Dimension, Bool)
stateP = toS <$> integer <*> integer <*> boolean where
  toS s d a = (s, d, a == '1')

transP :: Parser (Loc, Label, Form, Loc, [Var])
transP = toT <$> integer <*> integer <*> formP <* arrow <*> integer <*> many var where
  toT s a f t vs = (s, a, f, t, vs)

litP :: Parser Form
litP = toL <$> var <*> lexeme asciiChar <*> var where
  toL v1 c v2 = Lit c v1 v2

formP :: Parser Form
formP = makeExprParser bTerm bOperators where
  bOperators =
    [ [ Prefix (Not <$ symbol "not") ]
    , [ InfixL (And <$ symbol "and")
    ,   InfixL (Or  <$ symbol "or" ) ]
    ]
  bTerm = parens formP <|> litP

p :: Parser FAutomatonDescr
p = do
  symbol "FAutomaton"
  alphSize <- integer
  statesSize <- integer
  symbol "Alphabet"
  alph <- count alphSize alphP
  symbol "States"
  locations <- count statesSize stateP
  symbol "Delta"
  trans <- many transP
  return FAutomaton {..}


formToNLambda :: Form -> [Atom] -> [Atom] -> Formula
formToNLambda (Lit c (b1, n1) (b2, n2)) xs ys = op c (xys b1 !! n1) (xys b2 !! n2)
  where
    xys False = xs
    xys True = ys
    op '<' = lt
    op '=' = eq
    op '>' = gt
formToNLambda (And f1 f2) xs ys = formToNLambda f1 xs ys /\ formToNLambda f2 xs ys
formToNLambda (Or f1 f2) xs ys = formToNLambda f1 xs ys \/ formToNLambda f2 xs ys
formToNLambda (Not f) xs ys = not (formToNLambda f xs ys)

varsToNLambda vars xs ys = [xys b !! n | (b, n) <- vars]
  where
    xys False = xs
    xys True = ys

descriptionToNLambda :: FAutomatonDescr -> Automaton (Int, [Atom]) (Int, [Atom])
descriptionToNLambda FAutomaton{..} = Automaton{..} where
  states = unions [map (\w -> (l, w)) (replicateAtoms d) | (l, d, _) <- locations]
  alphabet = unions [map (\w -> (l, w)) (replicateAtoms d) | (l, d) <- alph]
  sDim = Map.fromList [(l, replicateAtoms d) | (l, d, _) <- locations]
  aDim = Map.fromList [(l, replicateAtoms d) | (l, d) <- alph]
  delta = unions [pairsWithFilter (\xs ys -> maybeIf (formToNLambda phi xs ys) ((s, xs), (l, ys), (t, varsToNLambda vars xs ys))) (sDim ! s) (aDim ! l) | (s, l, phi, t, vars) <- trans]
  initialStates = NL.empty
  finalStates = unions [map (\w -> (l, w)) (replicateAtoms d) | (l, d, b) <- locations, b]

printFormulaAutomaton file = do
  result <- runParser p file <$> readFile file
  case result of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right autDescription -> do
      print (descriptionToNLambda autDescription)

minimizeFormulaAutomaton file = do
  result <- runParser p file <$> readFile file
  case result of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right autDescription -> do
      print (minimize (descriptionToNLambda autDescription))
