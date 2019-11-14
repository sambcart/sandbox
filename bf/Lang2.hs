module Lang2 where

-- import Control.Applicative
-- import Control.Monad
import Control.Monad.State
import Control.Monad.Writer


data Command
  -- = OpenLoop | CloseLoop
  = IncrPtr  | DecrPtr
  | IncrVal  | DecrVal
  | Output
  deriving (Show, Eq)

data Term
  = Loop [Term]
  | Cmd Command
  deriving (Show, Eq)

lex' :: String -> [Command]
lex' = map lex1 . filter (`elem` "[]><+-,.")
  where lex1 c = case c of
          -- '[' -> OpenLoop
          -- ']' -> CloseLoop
          '>' -> IncrPtr
          '<' -> DecrPtr
          '+' -> IncrVal
          '-' -> DecrVal
          '.' -> Output

-- parseLoop :: [Command] -> ([Term],[Term])
-- parseLoop = go 0 []
--   where go _ l []     = error "Parse error: missing right-bracket."
--         go n l (c:cs) = case c of
--           CloseLoop
--             | n < 0     -> error "Parse error: off-balance brackets."
--             | n == 0    -> (parseCmds (reverse l), parseCmds cs)
--             | otherwise -> go (n - 1) (c:l) cs
--           OpenLoop      -> go (n + 1) (c:l) cs
--           _             -> go n (c:l) cs

parseCmds :: [Command] -> [Term]
parseCmds []     = []
parseCmds (c:cs) = case c of
  -- OpenLoop -> Loop l : cs' where (l,cs') = parseLoop cs
  _        -> Cmd c : cs' where cs' = parseCmds cs

parse :: String -> [Term]
parse = parseCmds . lex'


-- runCmds :: forall a. [Command] -> State Tape a
-- runCmds (c:cs) = do
--   case c of
--     IncrPtr -> runIncrPtr
--     DecrPtr -> runDecrPtr
--     IncrVal -> runIncrVal
--     DecrVal -> runDecrVal
--     Output  -> runOutput
--   runCmds cs

-- foo = flip runState emptyTape $ do
--   runIncrVal
--   runIncrVal
--   runIncrPtr
--   runIncrVal
--   runDecrPtr
--   runDecrVal
--   runOutput


foo :: Monoid a => [Tape -> Writer a Tape] -> (Tape, a)
foo = runWriter . foldM (flip ($)) emptyTape


infixr 6 :>
infixl 6 :<

data LList a = LNil | (:<) (LList a) a
  deriving Eq

data RList a = RNil | (:>) a (RList a)
  deriving Eq

fromLList :: LList a -> [a]
fromLList = reverse . go
  where go LNil     = []
        go (l :< x) = x : go l

fromRList :: RList a -> [a]
fromRList RNil     = []
fromRList (x :> r) = x : fromRList r

instance Show a => Show (LList a) where
  show = show . fromLList

instance Show a => Show (RList a) where
  show = show . fromRList



type Tape = (LList Int, RList Int)


emptyTape :: Tape
emptyTape = (LNil, RNil)

runIncrPtr :: Tape -> Writer [Int] Tape
runIncrPtr = return . go
  where go (l, RNil)   = (l :< 0, RNil)
        go (l, x :> r) = (l :< x, r)

runDecrPtr :: Tape -> Writer [Int] Tape
runDecrPtr = return . go
  where go (l :< x, r) = (l, x :> r)

runIncrVal :: Tape -> Writer [Int] Tape
runIncrVal = return . go
  where go (l, RNil)   = (l, 1 :> RNil)
        go (l, x :> r) = (l, (x + 1) :> r)

runDecrVal :: Tape -> Writer [Int] Tape
runDecrVal = return . go
  where go (l, x :> r) | x > 0 = (l, (x - 1) :> r)

runOutput :: Tape -> Writer [Int] Tape
runOutput t@(l,r) = tell [s] >> return t'
  where (s,t') = case r of
          RNil   -> (0, (l, 0 :> RNil))
          x :> r -> (x, t)


-- runIncrPtr :: State Tape ()
-- runIncrPtr = state $ \t -> ((), go t)
--   where go (l, RNil)   = (l :< 0, RNil)
--         go (l, x :> r) = (l :< x, r)

-- runDecrPtr :: State Tape ()
-- runDecrPtr = state $ \t -> ((), go t)
--   where go (l :< x, r) = (l, x :> r)

-- runIncrVal :: State Tape ()
-- runIncrVal = state $ \t -> ((), go t)
--   where go (l, RNil)   = (l, 1 :> RNil)
--         go (l, x :> r) = (l, (x + 1) :> r)

-- runDecrVal :: State Tape ()
-- runDecrVal = state $ \t -> ((), go t)
--   where go (l, x :> r) = (l, (x - 1) :> r)

-- runOutput :: State Tape Int
-- runOutput = state $ \t@(l,r) ->
--   case r of
--     RNil   -> (0,t)
--     x :> r -> (x,t)


testPrg :: String
testPrg = "++>+++++[<+>-]++++++++[<++++++>-]<."



