{-# LANGUAGE TupleSections #-}

module DPLL 
  ( solve
  , pos , neg
  , true , false
  , Model(..)
  , Constraint
  , Literal
  , Clause
  , CNF
  ) where

import Control.Monad               ( guard, mplus )
import Control.Monad.Trans.Writer  ( execWriterT, WriterT(..) )
import Data.List                   ( nub, unionBy )
import Data.Maybe                  ( mapMaybe )
import Data.Foldable               ( foldrM )
import Data.Function               ( on )

type Constraint sym  = (sym, Bool)
type Literal sym     = (sym, Bool)
type Clause sym      = [Literal sym]
type CNF sym         = [Clause sym]
type DPLL sym        = WriterT (Model sym) Maybe (CNF sym)

newtype Model sym    = Model { runModel :: [Constraint sym] }
  deriving (Show)

instance Eq sym => Semigroup (Model sym) where
  Model l <> Model r = Model (unionBy eq l r)
    where eq = (==) `on` fst

instance Eq sym => Monoid (Model sym) where
  mempty = Model []

solve :: Eq sym => CNF sym -> Maybe (Model sym)
solve cnf = execWriterT $ dpll (symbols cnf) cnf

dpll :: Eq sym => [sym] -> CNF sym -> DPLL sym
dpll _    []  = success
dpll syms cnf = propagateUnits cnf
            >>= eliminatePures syms
            >>= branchWithChoice syms

pos, neg :: sym -> Literal sym
pos = (, True)
neg = (, False)

true, false :: sym -> Constraint sym
true  = (, True)
false = (, False)

symbols :: Eq sym => CNF sym -> [sym]
symbols = nub . map fst . concat

literals :: Eq sym => [sym] -> CNF sym -> [(sym, [Bool])]
literals syms cnf = [(sym, mapMaybe (lookup sym) cnf) | sym <- syms]

pureLiterals :: Eq sym => [sym] -> CNF sym -> Model sym
pureLiterals syms cnf = Model $ do
  (sym, signs) <- literals syms cnf
  guard (isSingleton signs)
  return (sym, head signs)

unitClauses :: Eq sym => CNF sym -> Model sym
unitClauses cnf = Model $ map head $ filter isSingleton cnf

isSingleton :: [a] -> Bool
isSingleton = (== 1) . length

propagateUnits :: Eq sym => CNF sym -> DPLL sym
propagateUnits cnf = cnf `evalWith` unitClauses cnf

eliminatePures :: Eq sym => [sym] -> CNF sym -> DPLL sym
eliminatePures syms cnf = cnf `evalWith` pureLiterals syms cnf

branchWithChoice :: Eq sym => [sym] -> CNF sym -> DPLL sym
branchWithChoice syms cnf = chooseFalse `mplus` chooseTrue
  where chooseFalse = chooseNext False syms cnf
        chooseTrue = chooseNext True syms cnf

chooseNext :: Eq sym => Bool -> [sym] -> CNF sym -> DPLL sym
chooseNext val (sym:syms) cnf = cnf `evalWithConstraint` (sym, val) >>= dpll syms
chooseNext val []         cnf = success

success :: Eq sym => DPLL sym
success = return []

applyConstraintIn :: Eq sym => Constraint sym -> Clause sym -> Maybe (Clause sym)
applyConstraintIn (sym, val) clause =
  case lookup sym clause of
    Just val'
      | val == val' -> Nothing
      | otherwise   -> Just (filter ((/= sym) . fst) clause)
    Nothing         -> Just clause

applyConstraint :: Eq sym => Constraint sym -> CNF sym -> Maybe (CNF sym)
applyConstraint constraint []           = Just []
applyConstraint constraint (clause:cnf) = 
  case applyConstraintIn constraint clause of
    Just result
      | null result -> Nothing
      | otherwise   -> (result:) <$> applyConstraint constraint cnf
    Nothing         -> applyConstraint constraint cnf

evalWithConstraint :: Eq sym => CNF sym -> Constraint sym -> DPLL sym
evalWithConstraint cnf constraint = cnf `evalWith` Model [constraint]

evalWith :: Eq sym => CNF sym -> Model sym -> DPLL sym
evalWith cnf model = WriterT $ (, model) <$> foldrM applyConstraint cnf (runModel model)

