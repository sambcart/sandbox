module Lang where

import Control.Monad
import Data.Char


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


tokenize :: String -> [Operator]
tokenize = map tok1 . filter (`elem` "[]><+-,.")
  where
  tok1 c = case c of
    '[' -> OpenLoop
    ']' -> CloseLoop
    '>' -> IncrPtr
    '<' -> DecrPtr
    '+' -> IncrVal
    '-' -> DecrVal
    '.' -> Output

parseLoop :: [Operator] -> ([Command],[Command])
parseLoop = go 0 []
  where
  go _ l []     = error "Parse error: missing right-bracket."
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
parse = parseCmds . tokenize

type MemArr a = ([a],a,[a])
type MemState a = StateWriter (MemArr a) [a] a

peekState :: MemState a
peekState = SWriter $ \t@(_,a,_) -> (a, t, [])

runCmd :: (Eq a, Num a) => Command -> MemState a
runCmd (Loop l) = runCmds l
runCmd (Op c) = SWriter $ \t@(l,a,r) ->
  case c of
    IncrPtr ->
      case r of
        []  -> (0, (a:l, 0, []), [])
        b:r -> (b, (a:l, b, r ), [])
    DecrPtr ->
      case l of
        []  -> (0, ([], 0, a:r), [])
        b:l -> (b, (l,  b, a:r), [])
    IncrVal -> (a + 1, (l, a + 1, r), [])
    DecrVal -> (a - 1, (l, a - 1, r), [])
    Output  -> (a, t, [a])

runCmds :: (Eq a, Num a) => [Command] -> MemState a
runCmds []     = peekState
runCmds (c:cs) = case c of
  Op   _ -> runCmd c >> runCmds cs
  Loop l -> do
    a <- peekState
    if a == 0
    then runCmds cs
    else runCmds l >> runCmds (c:cs)

eval :: (Eq a, Num a) => [Command] -> (a, MemArr a, [a])
eval = flip runSWriter emptyS . runCmds
  where emptyS = ([],0,[])

evalState :: (Eq a, Num a) => [Command] -> a
evalState = fst . eval where fst (a,_,_) = a

evalMem :: (Eq a, Num a) => [Command] -> MemArr a
evalMem = snd . eval where snd (_,b,_) = b

evalOutput :: (Eq a, Num a) => [Command] -> [a]
evalOutput = thd . eval where thd (_,_,c) = c

runOutput :: (Eq a, Num a) => String -> [a]
runOutput = evalOutput . parse

runString :: String -> String
runString = map chr . runOutput

runIO :: String -> IO ()
runIO = putStr . runString


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
  (>>=) sw f = SWriter $ \s ->
    let (a, s0, w0) = runSWriter sw s
        (b, s1, w1) = runSWriter (f a) s0
    in (b, s1, w0 `mappend` w1)


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
