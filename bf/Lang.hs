module Lang where

import qualified Data.Map as Map

data Operator
  = IncrPtr | DecrPtr
  | IncrVal | DecrVal
  | Input   | Output

data Token
  = OpenLoop
  | CloseLoop
  | Op Operator

data Command
  = Loop [Command]
  | Cmd Operator

data Mem a = Mem (Map.Map Int a) Int

tokenize :: String -> [Token]
tokenize str = [chr2tok c | c <- str, c `elem` "[]<>-+,."]
  where chr2tok c = case c of
          { '[' -> OpenLoop   ; ']' -> CloseLoop
          ; '>' -> Op IncrPtr ; '<' -> Op DecrPtr
          ; '+' -> Op IncrVal ; '-' -> Op DecrVal
          ; ',' -> Op Input   ; '.' -> Op Output
          }

parse :: [Token] -> [Command]
parse []     = []
parse (t:ts) = case t of
  Op op     -> Cmd op : parse ts
  CloseLoop -> error "Parse error: extra right-bracket(s)."
  OpenLoop  -> Loop (parse lp) : parse ts'
    where (lp,ts') = go 0 [] ts
          go _ lp []     = error "Parse error: missing right-bracket(s)."
          go n lp (t:ts) = case t of
            Op _     -> go n (t:lp) ts
            OpenLoop -> go (n + 1) (t:lp) ts
            CloseLoop | n <  0    -> error "Parse error: off-balance bracket(s)."
                      | n == 0    -> (reverse lp, ts)
                      | otherwise -> go (n - 1) (t:lp) ts

run :: (Read a, Sym a) => String -> IO (Mem a)
run = go (Mem Map.empty 0) . parse . tokenize
  where shiftl (Mem arr ptr) = Mem arr (ptr - 1)
        shiftr (Mem arr ptr) = Mem arr (ptr + 1)
        put (Mem arr ptr) a = Mem (Map.insert ptr a arr) ptr
        get (Mem arr ptr) = maybe empty id (Map.lookup ptr arr)
        go m []     = return m
        go m (c:cs) = case c of
          Cmd IncrPtr -> go (shiftr m) cs
          Cmd DecrPtr -> go (shiftl m) cs
          Cmd IncrVal -> go (put m $ incr $ get m) cs
          Cmd DecrVal -> go (put m $ decr $ get m) cs
          Cmd Input   -> getLine >>= flip go cs . put m . read
          Cmd Output  -> putStr (toString $ get m) >> go m cs
          Loop lp | get m == empty -> go m cs
                  | otherwise      -> go m lp >>= flip go (c:cs)

class (Bounded a, Enum a, Eq a) => Sym a where
  toString :: a -> String
  incr, decr :: a -> a
  incr a = if a == maxBound then minBound else succ a
  decr a = if a == minBound then maxBound else pred a
  empty :: a
  empty = minBound
