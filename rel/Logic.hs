module Logic where

import Control.Monad
import Control.Applicative
import Lang
import Subst

newtype LogicM a = LogicM { runM :: Subst -> [(Subst, a)] }

instance Functor LogicM where
  fmap = liftM

instance Applicative LogicM where
  pure  = return
  (<*>) = ap

instance Monad LogicM where
  return a  = LogicM $ \s -> [(s, a)]
  (>>=) m f = LogicM $ \s -> concatMap r $ runM m s
    where r (s, x) = runM (f x) s

instance Alternative LogicM where
  empty     = LogicM $ \s -> []
  (<|>) a b = LogicM $ \s -> runM a s ++ runM b s

type Goal = LogicM ()

success, failure :: Goal
success = return ()
failure = empty

run :: Termed a => LogicM a -> [Term]
run p = [reify r s | (s, r) <- runM p emptySubst]

runN :: Termed a => Int -> LogicM a -> [Term]
runN n = take n . run

fresh :: LogicM Term
fresh = LogicM $ \(Subst m i c) -> [(Subst m (i + 1) c, Var i)]

freshen :: Int -> LogicM [Term]
freshen n = replicateM n fresh

(===) :: (Termed a, Termed b) => a -> b -> Goal
(===) a b = LogicM $ \s ->
  let t = toTerm a
      u = toTerm b in
  case unify t u s of
    Nothing             -> []
    Just s' | check s'  -> [(s', ())]
            | otherwise -> []

(=/=) :: (Termed a, Termed b) => a -> b -> Goal
(=/=) a b = LogicM $ \s ->
  let t = toTerm a
      u = toTerm b in
  case unify t u s of
    Nothing             -> [(s, ())]
    Just s' | s == s'   -> []
            | otherwise -> [(extend t u s, ())]

conde :: [Goal] -> Goal
conde = foldr (<|>) failure

conda :: [Goal] -> Goal
conda = foldr (>>) success

condap :: (a -> Goal) -> [a] -> Goal
condap f = foldr ((>>) . f) success
