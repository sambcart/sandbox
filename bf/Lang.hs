module Lang where
import Data.Char
import Data.Word
import qualified Data.Map as M

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

tokenize :: String -> [Token]
tokenize = map chr2tok . filter (`elem` "[]<>-+,.")
  where
  chr2tok c = case c of
    '[' -> OpenLoop
    ']' -> CloseLoop
    '>' -> Op IncrPtr
    '<' -> Op DecrPtr
    '+' -> Op IncrVal
    '-' -> Op DecrVal
    ',' -> Op Input
    '.' -> Op Output

parseLoop :: [Token] -> ([Command],[Command])
parseLoop = go 0 []
  where
  go _ lp []     = error "Parse error: missing right-bracket."
  go n lp (t:ts) = case t of
    CloseLoop
      | n <  0    -> error "Parse error: off-balance brackets."
      | n == 0    -> (parseTokens (reverse lp), parseTokens ts)
      | otherwise -> go (n - 1) (t:lp) ts
    OpenLoop      -> go (n + 1) (t:lp) ts
    _             -> go n (t:lp) ts

parseTokens :: [Token] -> [Command]
parseTokens []     = []
parseTokens (t:ts) = case t of
  CloseLoop -> error "Parse error: extra right-bracket."
  OpenLoop  -> Loop lp : cs where (lp,cs) = parseLoop ts
  Op op     -> Cmd  op : cs where cs = parseTokens ts

parseString :: String -> [Command]
parseString = parseTokens . tokenize

class Eq a => Sym a where
  toString   :: a -> String
  -- fromString :: String -> a
  incr       :: a -> a
  decr       :: a -> a
  empty      :: a

instance Sym Word8 where
  toString n = [chr $ fromIntegral n]
  -- fromString s = 
  incr n = if n == 255 then 0 else n + 1
  decr n = if n == 0 then 255 else n - 1
  empty = 0

instance Sym Bool where
  toString b = if b then "1" else "0"
  -- fromString s = 
  incr = not
  decr = not
  empty = False

data Mem a = Mem (M.Map Int a) Int

shiftl, shiftr :: Mem a -> Mem a
shiftl (Mem arr ptr) = Mem arr (ptr - 1)
shiftr (Mem arr ptr) = Mem arr (ptr + 1)

put :: Sym a => Mem a -> a -> Mem a
put (Mem arr ptr) a = Mem (M.insert ptr a arr) ptr

get :: Sym a => Mem a -> a
get (Mem arr ptr) = maybe empty id (M.lookup ptr arr)

run :: (Read a, Sym a) => String -> IO (Mem a)
run str = go (parseString str) (Mem M.empty 0)
  where
  go []     r = return r
  go (c:cs) r = case c of
    Loop lp -> if get r == empty
      then go cs r
      else go lp r >>= go (c:cs)
    Cmd IncrPtr -> go cs (shiftr r)
    Cmd DecrPtr -> go cs (shiftl r)
    Cmd IncrVal -> go cs (put r $ incr $ get r)
    Cmd DecrVal -> go cs (put r $ decr $ get r)
    Cmd Input   -> read <$> getLine >>= go cs . put r
    Cmd Output  -> putStr (toString $ get r) >> go cs r

test1 = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
test2 = ",.[>+<[-]]+>[<->-]<."