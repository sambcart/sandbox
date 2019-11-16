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
  go n lp (c:cs) = case c of
    CloseLoop
      | n <  0    -> error "Parse error: off-balance brackets."
      | n == 0    -> (parseTokens (reverse lp), parseTokens cs)
      | otherwise -> go (n - 1) (c:lp) cs
    OpenLoop      -> go (n + 1) (c:lp) cs
    _             -> go n (c:lp) cs

parseTokens :: [Token] -> [Command]
parseTokens []     = []
parseTokens (t:ts) = case t of
  CloseLoop -> error "Parse error: extra right-bracket."
  OpenLoop  -> Loop lp : cs where (lp,cs) = parseLoop ts
  Op op     -> Cmd  op : cs where cs = parseTokens ts

parseString :: String -> [Command]
parseString = parseTokens . tokenize

incr, decr :: Word8 -> Word8
incr n = if n == 255 then 0 else n + 1
decr n = if n == 0 then 255 else n - 1

toString :: Word8 -> String
toString = (:[]) . chr . fromIntegral

data Mem = Mem (M.Map Int Word8) Int

shiftl, shiftr :: Mem -> Mem
shiftl (Mem arr ptr) = Mem arr (ptr-1)
shiftr (Mem arr ptr) = Mem arr (ptr+1)

put :: Mem -> Word8 -> Mem
put (Mem arr ptr) a = Mem (M.insert ptr a arr) ptr

get :: Mem -> Word8
get (Mem arr ptr) = maybe 0 id (M.lookup ptr arr)

run :: String -> IO Mem
run str = go (parseString str) (Mem M.empty 0)
  where
  go []     r = return r
  go (c:cs) r = case c of
    Loop lp -> if get r == 0
      then go cs r
      else go lp r >>= go (c:cs)
    Cmd IncrPtr -> go cs (shiftr r)
    Cmd DecrPtr -> go cs (shiftl r)
    Cmd IncrVal -> let v = get r in go cs (put r $ incr v)
    Cmd DecrVal -> let v = get r in go cs (put r $ decr v)
    Cmd Input   -> read <$> getLine >>= go cs . put r
    Cmd Output  -> putStr (toString $ get r) >> go cs r

test = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
