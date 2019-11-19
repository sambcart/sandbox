{-# LANGUAGE LambdaCase #-}
module Logic where
import Control.Applicative
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map


class MonadPlus m => LogicM m where
  msplit     :: m a -> m (Maybe (a, m a))
  interleave :: m a -> m a -> m a
  (>>-)      :: m a -> (a -> m b) -> m b
  ifte       :: m a -> (a -> m b) -> m b -> m b
  once       :: m a -> m a

  interleave sg1 sg2 = msplit sg1 >>=
    \case { Nothing -> sg2
          ; Just (sg11, sg12) ->
            return sg11 `mplus` interleave sg2 sg12
          }

  sg >>- f = msplit sg >>=
    \case { Nothing -> mzero
          ; Just (sg1, sg2) ->
            interleave (f sg1) (sg2 >>- f)
          }
  
  ifte t th el = msplit t >>=
    \case { Nothing -> el
          ; Just (sg1, sg2) ->
            th sg1 `mplus` (sg2 >>= th)
          }
  
  once m = msplit m >>=
    \case { Nothing -> mzero
          ; Just (sg1, _) ->
            return sg1
          }



instance LogicM [] where
  msplit []     = mempty
  msplit (g:sg) = return (Just (g, sg))

evens :: MonadPlus m => m Int
evens = return 0 `mplus` (evens >>= return . (2+))

odds :: MonadPlus m => m Int
odds = return 1 `mplus` (odds >>= return . (2+))


