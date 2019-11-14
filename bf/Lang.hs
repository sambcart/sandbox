{-# LANGUAGE LambdaCase #-}

module Lang2 where

import Prelude hiding (lex)
import Data.Char (chr)


data Operator
  = OpenLoop | CloseLoop
  | IncrPtr  | DecrPtr
  | IncrVal  | DecrVal
  | Output
  deriving (Show, Eq)

data Command
  = Op Operator
  | Loop [Command]
  deriving (Show, Eq)

lex :: String -> [Operator]
lex = map lex1 . filter (`elem` "[]><+-,.")
  where lex1 c = case c of
          '[' -> OpenLoop
          ']' -> CloseLoop
          '>' -> IncrPtr
          '<' -> DecrPtr
          '+' -> IncrVal
          '-' -> DecrVal
          '.' -> Output

parseLoop :: [Operator] -> ([Command],[Command])
parseLoop = go 0 []
  where go _ l []     = error "Parse error: missing right-bracket."
        go n l (c:cs) = case c of
          CloseLoop
            | n < 0     -> error "Parse error: off-balance brackets."
            | n == 0    -> (parseCmds (reverse l), parseCmds cs)
            | otherwise -> go (n - 1) (c:l) cs
          OpenLoop      -> go (n + 1) (c:l) cs
          _             -> go n (c:l) cs

parseCmds :: [Operator] -> [Command]
parseCmds []     = []
parseCmds (c:cs) = case c of
  OpenLoop -> Loop l : cs' where (l,cs') = parseLoop cs
  _        -> Op   c : cs' where cs' = parseCmds cs

parse :: String -> [Command]
parse = parseCmds . lex


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


data LList a = LNil | LSnoc (LList a) a
data RList a = RNil | RCons a (RList a)
type Tape  a = (LList a, a, RList a)

fromLList :: LList a -> [a]
fromLList = reverse . go
  where go LNil        = []
        go (LSnoc l a) = a : go l

fromRList :: RList a -> [a]
fromRList RNil        = []
fromRList (RCons a r) = a : fromRList r

instance Show a => Show (LList a) where
  show = show . fromLList

instance Show a => Show (RList a) where
  show = show . fromRList


type MemArr = Tape Int
type MemState = StateWriter MemArr [Int] Int


peekState :: MemState
peekState = SWriter $ \t@(_,a,_) -> (a, t, [])

runCmd :: Command -> MemState
runCmd (Loop l) = runCmds l
runCmd (Op c) = case c of
  IncrPtr -> SWriter $ \case
    (l, a, RNil)      -> (0, (LSnoc l a, 0, RNil), [])
    (l, a, RCons b r) -> (b, (LSnoc l a, b, r),    [])
  DecrPtr -> SWriter $ \case
    (LNil, a, r)      -> (0, (LNil, 0, RCons a r), [])
    (LSnoc l a, b, r) -> (a, (l,    a, RCons b r), [])
  IncrVal -> SWriter $ \(l,a,r) -> let a' = a + 1 in (a', (l,a',r), [])
  DecrVal -> SWriter $ \(l,a,r) -> let a' = a - 1 in (a', (l,a',r), [])
  Output  -> SWriter $ \t@(_,a,_) -> (a, t, [a])

runCmds :: [Command] -> MemState
runCmds []     = peekState
runCmds (c:cs) = case c of
  Op _ -> runCmd c >> runCmds cs
  Loop l -> do
    a <- peekState
    if a == 0
    then runCmds cs
    else runCmds l >> runCmds (c:cs)


eval :: [Command] -> (Int, MemArr, [Int])
eval cmds = runSWriter (runCmds cmds) emptyMem
  where emptyMem = (LNil, 0, RNil)

evalState :: [Command] -> Int
evalState cmds = st where (st,_,_) = eval cmds

evalMem :: [Command] -> MemArr
evalMem cmds = mem where (_,mem,_) = eval cmds

evalOut :: [Command] -> [Int]
evalOut cmds = out where (_,_,out) = eval cmds

runString :: String -> String
runString = map chr . evalOut . parse

runIO :: String -> IO ()
runIO = putStr . runString


helloWorld :: String
helloWorld = "++++++++"
          ++ "[>++++"
          ++ "[>++>+++>+++>+<<<<-]"
          ++ ">+>+>->>+[<]<-]"
          ++ ">>."
          ++ ">---."
          ++ "+++++++.."
          ++ "+++."
          ++ ">>."
          ++ "<-."
          ++ "<."
          ++ "+++."
          ++ "------."
          ++ "--------."
          ++ ">>+."
          ++ ">++."

-- runString helloWorld == "Hello world!\n"

