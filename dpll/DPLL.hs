{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module DPLL where

import Control.Monad
import Control.Monad.Trans.Writer
import Data.Maybe    (catMaybes)
import Data.Foldable (foldrM)
import Data.Function (on)
import Data.List     (nub, unionBy)


type Constraint sym  = (sym, Bool)
type Literal sym     = (sym, Sign)
type Clause sym      = [Literal sym]
type CNF sym         = [Clause sym]
type DPLL sym        = WriterT (Model sym) Maybe (CNF sym)

data Sign = Neg | Pos
  deriving (Show, Eq)

data Model sym = Model { runModel :: [Constraint sym] }
  deriving (Show)


satisfy :: Eq sym => CNF sym -> Maybe (Model sym)
satisfy cnf = execWriterT $ dpll (symbols cnf) cnf

dpll :: Eq sym => [sym] -> CNF sym -> DPLL sym
dpll _    []  = success
dpll syms cnf = do
  cnf1 <- propagateUnits cnf
  cnf2 <- eliminatePures syms cnf1
  msum [ chooseNextFalse syms cnf2
       , chooseNextTrue syms cnf2
       ]


toConstraint :: Literal sym -> Constraint sym
toConstraint (sym, sign) = (sym, toBool sign)

toBool :: Sign -> Bool
toBool Neg = False
toBool Pos = True

true, false :: sym -> Constraint sym
true  = (, True)
false = (, False)

pos, neg :: sym -> Literal sym
pos = (, Pos)
neg = (, Neg)

symbols :: Eq sym => CNF sym -> [sym]
symbols = nub . map fst . concat

removeSymbol :: Eq sym => sym -> Clause sym -> Clause sym
removeSymbol sym = filter ((/= sym) . fst)

signsIn :: Eq sym => sym -> CNF sym -> [Sign]
signsIn sym = catMaybes . map (lookup sym)

literals :: Eq sym => [sym] -> CNF sym -> [(sym, [Sign])]
literals syms cnf = [(sym, signsIn sym cnf) | sym <- syms]

pureLiterals :: Eq sym => [sym] -> CNF sym -> Model sym
pureLiterals syms cnf = Model $ map toConstraint $ do
  (sym, signs) <- literals syms cnf
  guard (length signs == 1)
  return (sym, head signs)

unitClauses :: Eq sym => CNF sym -> Model sym
unitClauses = Model . map toConstraint . concat . filter ((== 1) . length)

propagateUnits :: Eq sym => CNF sym -> DPLL sym
propagateUnits cnf = cnf `evalWith` unitClauses cnf

eliminatePures :: Eq sym => [sym] -> CNF sym -> DPLL sym
eliminatePures syms cnf = cnf `evalWith` pureLiterals syms cnf

chooseNextFalse :: Eq sym => [sym] -> CNF sym -> DPLL sym
chooseNextFalse (sym:syms) cnf = (cnf `evalWithConstraint` false sym) >>= dpll syms
chooseNextFalse []         cnf = success

chooseNextTrue :: Eq sym => [sym] -> CNF sym -> DPLL sym
chooseNextTrue (sym:syms) cnf = (cnf `evalWithConstraint` true sym) >>= dpll syms
chooseNextTrue []         cnf = success

success :: Eq sym => DPLL sym
success = return []


applyConstraintIn :: Eq sym => Constraint sym -> Clause sym -> Maybe (Clause sym)
applyConstraintIn (sym, val) clause =
  case lookup sym clause of
    Just (toBool -> val')
      | val == val' -> Nothing
      | otherwise   -> Just (removeSymbol sym clause)
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


instance Eq sym => Semigroup (Model sym) where
  Model l <> Model r = Model (unionBy ((==) `on` fst) l r)

instance Eq sym => Monoid (Model sym) where
  mempty = Model []


-- satisfy exampleCNF
-- >>> Just [(1,True),(4,True),(2,True),(3,True)]

exampleCNF :: CNF Int
exampleCNF =
  [ [ neg 1, pos 2, pos 3 ]
  , [ pos 1, pos 3, pos 4 ]
  , [ pos 1, pos 3, neg 4 ]
  , [ pos 1, neg 3, pos 4 ]
  , [ pos 1, neg 3, neg 4 ]
  , [ neg 2, neg 3, pos 4 ]
  , [ neg 1, pos 2, neg 3 ]
  , [ neg 1, neg 2, pos 3 ]
  ]

