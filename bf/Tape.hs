{-# LANGUAGE LambdaCase #-}

module Tape where


newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap f wr = Writer (f a, w)
    where (a, w) = runWriter wr

instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  (<*>) wr0 wr1 = Writer (f a, w0 `mappend` w1)
    where (f, w0) = runWriter wr0
          (a, w1) = runWriter wr1

instance Monoid w => Monad (Writer w) where
  return = pure
  (>>=) wr0 f = Writer (b, w0 `mappend` w1)
    where (a, w0) = runWriter wr0
          (b, w1) = runWriter (f a)

tell :: w -> Writer w ()
tell w = Writer ((), w)


newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f st = State $ \s -> f' (runState st s)
    where f' (a, s) = (f a, s)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (<*>) st0 st1 = State $ \s ->
    let (f, s0) = runState st0 s
        (a, s1) = runState st1 s0
    in (f a, s1)

instance Monad (State s) where
  return = pure
  (>>=) st0 f = State $ \s ->
    let (a, s0) = runState st0 s
        st1     = f a
    in runState st1 s0


newtype StateWriter s w a = SWriter { runSWriter :: s -> (a, s, w) }

instance Functor (StateWriter s w) where
  fmap f sw = SWriter $ \s -> f' (runSWriter sw s)
    where f' (a, s, w) = (f a, s, w)

instance Monoid w => Applicative (StateWriter s w) where
  pure a = SWriter $ \s -> (a, s, mempty)
  (<*>) sw0 sw1 = SWriter $ \s ->
    let (f, s0, w0) = runSWriter sw0 s
        (a, s1, w1) = runSWriter sw1 s0
    in (f a, s1, w0 `mappend` w1)

instance Monoid w => Monad (StateWriter s w) where
  return = pure
  (>>=) sw0 f = SWriter $ \s ->
    let (a, s0, w0) = runSWriter sw0 s
        (b, s1, w1) = runSWriter (f a) s0
    in (b, s1, w0 `mappend` w1)



type Stack = [Int]

pop :: StateWriter Stack [String] Int
pop = SWriter $ \(n:s) -> (n, s, ["POP " ++ show n])

push :: Int -> StateWriter Stack [String] ()
push n = SWriter $ \s -> ((), n:s, ["PUSH " ++ show n])

stackTest :: StateWriter Stack [String] Int
stackTest = do
  push 3
  push 4
  push 5
  pop



infixl 6 :<
infixr 6 :>

data LList a = LNil | (:<) (LList a) a
data RList a = RNil | (:>) a (RList a)

fromLList :: LList a -> [a]
fromLList = reverse . go
  where go LNil     = []
        go (l :< a) = a : go l

fromRList :: RList a -> [a]
fromRList RNil     = []
fromRList (a :> r) = a : fromRList r

instance Show a => Show (LList a) where
  show = show . fromLList

instance Show a => Show (RList a) where
  show = show . fromRList

type Tape a = (LList a, a, RList a)


get :: StateWriter (Tape a) [a] a
get = SWriter $ \s@(_,a,_) -> (a, s, [a])

shiftr :: a -> StateWriter (Tape a) [a] a
shiftr dflt = SWriter $ \case
  (l, a, RNil)   -> (dflt, (l :< a, dflt, RNil), [])
  (l, a, b :> r) -> (b,    (l :< a, b, r),       [])

shiftl :: a -> StateWriter (Tape a) [a] a
shiftl dflt = SWriter $ \case
  (LNil, a, r)   -> (dflt, (LNil, dflt, a :> r), [])
  (l :< a, b, r) -> (a,    (l, a, b :> r),       [])

incr :: Enum a => StateWriter (Tape a) [a] a
incr = SWriter $ \(l,a,r) -> let a' = succ a in (a', (l,a',r), [])

decr :: Enum a => StateWriter (Tape a) [a] a
decr = SWriter $ \(l,a,r) -> let a' = pred a in (a', (l,a',r), [])

tapeTest :: StateWriter (Tape Int) [Int] Int
tapeTest = do
  incr     >> incr                              -- Set Cell #1 to 2
  shiftr 0 >> incr     >> incr >> incr >> get   -- Move to Cell #2 and set value to 3, returning the result
  shiftl 0 >> decr     >> get                   -- Move to Cell #1 and set value to 1, returning the result
  shiftr 0 >> shiftr 0 >> get                   -- Move to Cell #3 and return its value (0)

