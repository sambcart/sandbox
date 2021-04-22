{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad       ( forM, replicateM )
import Data.Ix             ( range )
import Data.Maybe          ( catMaybes )
import System.Environment  ( getArgs )
import System.Exit         ( ExitCode(..), exitSuccess, exitWith )
import System.Random       ( randomRIO )

import DPLL

-- $ ghc -o dpll DPLL.hs Main.hs
-- $ ./dpll [# of SYMBOLS] [# of CLAUSES]

randomMaybeBool :: IO (Maybe Bool)
randomMaybeBool = go <$> randomRIO (-1, 1 :: Int)
  where go 0 = Nothing
        go n = Just (n > 0)

randomClause :: Int -> IO (Clause Int)
randomClause symbolNum = catMaybes <$> forM symbols go
  where symbols = range (1, symbolNum)
        go sym = fmap (fmap (sym,)) randomMaybeBool

randomCNF :: Int -> Int -> IO (CNF Int)
randomCNF clauseNum = replicateM clauseNum . randomClause

readArgs :: [String] -> Maybe (Int, Int)
readArgs (s1:s2:_) = Just (read s1, read s2)
readArgs _         = Nothing

main :: IO ()
main = do
  maybeArgs <- readArgs <$> getArgs
  case maybeArgs of
    Nothing -> exitWith $ ExitFailure 1
    Just (symbolNum, clauseNum) -> do
      cnf <- randomCNF clauseNum symbolNum
      case solve cnf of
        Nothing -> putStrLn "UNSAT" >> exitSuccess
        Just model -> putStrLn "SAT" >> exitSuccess

