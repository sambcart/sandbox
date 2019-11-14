module Lang where

-- import Control.Applicative
-- import Control.Monad
import Control.Monad.State


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


infixr 6 :>
infixl 6 :<

data LTape a = LNil | (:<) (LTape a) a
  deriving Eq

data RTape a = RNil | (:>) a (RTape a)
  deriving Eq

fromLTape :: LTape a -> [a]
fromLTape = reverse . go
  where go LNil     = []
        go (l :< x) = x : go l

fromRTape :: RTape a -> [a]
fromRTape RNil     = []
fromRTape (x :> r) = x : fromRTape r

instance Show a => Show (LTape a) where
  show = show . fromLTape

instance Show a => Show (RTape a) where
  show = show . fromRTape



type Tape = (LTape Int, RTape Int)


emptyTape :: Tape
emptyTape = (LNil, RNil)


runIncrPtr :: State Tape ()
runIncrPtr = state $ \t -> ((), go t)
  where go (l, RNil)   = (l :< 0, RNil)
        go (l, x :> r) = (l :< x, r)

runDecrPtr :: State Tape ()
runDecrPtr = state $ \t -> ((), go t)
  where go (l :< x, r) = (l, x :> r)

runIncrVal :: State Tape ()
runIncrVal = state $ \t -> ((), go t)
  where go (l, RNil)   = (l, 1 :> RNil)
        go (l, x :> r) = (l, (x + 1) :> r)

runDecrVal :: State Tape ()
runDecrVal = state $ \t -> ((), go t)
  where go (l, x :> r) = (l, (x - 1) :> r)

runOutput :: State Tape Int
runOutput = state $ \t@(l,r) ->
  case r of
    RNil   -> (0,t)
    x :> r -> (x,t)


testPrg :: String
testPrg = "++>+++++[<+>-]++++++++[<++++++>-]<."



