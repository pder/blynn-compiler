ffi "putchar_cast" putChar :: Char -> IO ()
ffi "getargcount" getArgCount :: IO Int
ffi "getargchar" getArgChar :: Int -> Int -> IO Char
ffi "getchar_fp" getChar :: IO Int
ffi "reset_buffer" resetBuffer :: IO ()
ffi "put_buffer" putBuffer :: Int -> IO ()
ffi "stdin_load_buffer" stdinLoadBuffer :: IO ()

infixr 9 .
infixl 7 * , `div` , `mod`
infixl 6 + , -
infixr 5 ++
infixl 4 <*> , <$> , <* , *>
infix 4 == , /= , <=
infixl 3 && , <|>
infixl 2 ||
infixl 1 >> , >>=
infixr 0 $

fromIntegral = fromInteger . toInteger

-- Integer literals in `fromInt`, `fromInteger` or any of their dependencies
-- cause infinite loops, because our compilers apply `fromInt` or `fromInteger`
-- to integer literal. We get `0 :: Int` in a roundabout way:
zeroInt = ord '\0'

fromInt x
  | x == zeroInt = fromInteger $ Integer True []
  | True = fromInteger . Integer True . (:[]) $ wordFromInt x

class Ring a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  fromInteger :: Integer -> a
class Integral a where
  div :: a -> a -> a
  mod :: a -> a -> a
  quot :: a -> a -> a
  rem :: a -> a -> a
  toInteger :: a -> Integer
  -- divMod, quotRem

instance Ring Int where
  (+) = intAdd
  (-) = intSub
  (*) = intMul
  -- TODO: Negative case.
  fromInteger (Integer xsgn xs) = intFromWord $ fst $ mpView xs
instance Integral Int where
  div = intDiv
  mod = intMod
  quot = intQuot
  rem = intRem
  toInteger x
    | 0 <= x = Integer True $ if x == 0 then [] else [wordFromInt x]
    | True = Integer False [wordFromInt $ 0 - x]

instance Ring Word where
  (+) = wordAdd
  (-) = wordSub
  (*) = wordMul
  -- TODO: Negative case.
  fromInteger (Integer xsgn xs) = fst $ mpView xs
instance Integral Word where
  div = wordDiv
  mod = wordMod
  quot = wordQuot
  rem = wordRem
  toInteger 0 = Integer True []
  toInteger x = Integer True [x]
instance Eq Word where (==) = wordEq
instance Ord Word where (<=) = wordLE

data Word64 = Word64 Word Word deriving Eq
instance Ring Word64 where
  Word64 a b + Word64 c d = uncurry Word64 $ word64Add a b c d
  Word64 a b - Word64 c d = uncurry Word64 $ word64Sub a b c d
  Word64 a b * Word64 c d = uncurry Word64 $ word64Mul a b c d
  -- TODO: Negative case.
  fromInteger (Integer xsgn xs) = Word64 x y where
    (x, xt) = mpView xs
    (y, _) = mpView xt

instance Ord Word64 where
  Word64 a b <= Word64 c d
    | b == d = a <= c
    | True = b <= d

-- Multiprecision arithmetic.
data Integer = Integer Bool [Word] deriving Eq
instance Ring Integer where
  Integer xsgn xs + Integer ysgn ys
    | xsgn == ysgn = Integer xsgn $ mpAdd xs ys
    | True = case mpCompare xs ys of
      LT -> mpCanon ysgn $ mpSub ys xs
      _ -> mpCanon xsgn $ mpSub xs ys
  Integer xsgn xs - Integer ysgn ys
    | xsgn /= ysgn = Integer xsgn $ mpAdd xs ys
    | True = case mpCompare xs ys of
      LT -> mpCanon (not ysgn) $ mpSub ys xs
      _ -> mpCanon xsgn $ mpSub xs ys
  Integer xsgn xs * Integer ysgn ys = Integer (xsgn == ysgn) $ mpMul xs ys
  fromInteger = id

instance Integral Integer where
  -- TODO: Trucate `div` towards zero.
  div (Integer xsgn xs) (Integer ysgn ys) = mpCanon0 (xsgn == ysgn) $ fst $ mpDivMod xs ys
  mod (Integer xsgn xs) (Integer ysgn ys) = mpCanon0 ysgn $ snd $ mpDivMod xs ys
  quot (Integer xsgn xs) (Integer ysgn ys) = mpCanon0 (xsgn == ysgn) $ fst $ mpDivMod xs ys
  rem (Integer xsgn xs) (Integer ysgn ys) = mpCanon0 ysgn $ snd $ mpDivMod xs ys
  toInteger = id

instance Ord Integer where
  compare (Integer xsgn xs) (Integer ysgn ys)
    | xsgn = if ysgn then mpCompare xs ys else GT
    | True = if ysgn then LT else mpCompare ys xs

mpView [] = (wordFromInt zeroInt, [])
mpView (x:xt) = (x, xt)

mpCanon sgn xs = mpCanon0 sgn $ reverse $ dropWhile (0 ==) $ reverse xs
mpCanon0 sgn xs = case xs of
  [] -> Integer True []
  _ -> Integer sgn xs

mpCompare [] [] = EQ
mpCompare [] _  = LT
mpCompare _  [] = GT
mpCompare (x:xt) (y:yt) = case mpCompare xt yt of
  EQ -> compare x y
  o -> o

mpAdc [] [] c = ([], c)
mpAdc xs ys c = first (lo:) $ mpAdc xt yt hi where
  (x, xt) = mpView xs
  (y, yt) = mpView ys
  (lo,hi) = uncurry (word64Add c 0) $ word64Add x 0 y 0

mpAdd xs ys | c == 0 = zs
            | True = zs ++ [c]
  where (zs, c) = mpAdc xs ys 0

mpSub xs ys = fst $ mpSbb xs ys 0

mpSbb xs ys b = go xs ys b where
  go [] [] b = ([], b)
  go xs ys b = first (lo:) $ go xt yt $ 1 - hi where
    (x, xt) = mpView xs
    (y, yt) = mpView ys
    (lo,hi) = uncurry word64Sub (word64Sub x 1 y 0) b 0

mpMulWord _ []     c = if c == 0 then [] else [c]
mpMulWord x (y:yt) c = lo:mpMulWord x yt hi where
  (lo, hi) = uncurry (word64Add c 0) $ word64Mul x 0 y 0

mpMul [] _ = []
mpMul (x:xt) ys = case mpMulWord x ys 0 of
  [] -> []
  z:zs -> z:mpAdd zs (mpMul xt ys)

mpDivModWord xs y = first (reverse . dropWhile (0 ==)) $ go 0 $ reverse xs where
  go r [] = ([], r)
  go n (x:xt) = first (q:) $ go r xt where
    q = fst $ word64Div x n y 0
    r = fst $ word64Mod x n y 0

mpDivMod xs ys = first (reverse . dropWhile (== 0)) $ go us where
  s = mpDivScale $ last ys
  us = mpMulWord s (xs ++ [0]) 0
  vs = mpMulWord s ys 0
  (v1:vt) = reverse vs
  vlen = length vs
  go us | ulen <= vlen = ([], fst $ mpDivModWord us s)
        | True = first (q:) $ go $ lsbs ++ init ds
    where
    ulen = length us
    (u0:u1:ut) = reverse us
    (lsbs, msbs) = splitAt (ulen - vlen - 1) us
    (ql, qh) = word64Div u1 u0 v1 0
    q0  = if 1 <= qh then (0-1) else ql
    (q, ds) = foldr const undefined [(q, ds) | q <- iterate (- 1) q0, let (ds, bor) = mpSbb msbs (mpMulWord q vs 0) 0, bor == 0]

mpDivScale n = fst $ word64Div 0 1 (n + 1) 0

class Functor f where fmap :: (a -> b) -> f a -> f b
class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
(<$>) = fmap
liftA2 f x y = f <$> x <*> y
(>>) f g = f >>= \_ -> g
class Eq a where (==) :: a -> a -> Bool
instance Eq Int where (==) = intEq
instance Eq Char where (==) = charEq
($) f x = f x
id x = x
const x y = x
flip f x y = f y x
(&) x f = f x
data Ordering = LT | GT | EQ
class Ord a where
  (<=) :: a -> a -> Bool
  x <= y = case compare x y of
    LT -> True
    EQ -> True
    GT -> False
  compare :: a -> a -> Ordering
  compare x y = if x <= y then if y <= x then EQ else LT else GT

instance Ord Int where (<=) = intLE
instance Ord Char where (<=) = charLE
instance Ord a => Ord [a] where
  xs <= ys = case xs of
    [] -> True
    x:xt -> case ys of
      [] -> False
      y:yt -> case compare x y of
        LT -> True
        GT -> False
        EQ -> xt <= yt
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
fpair (x, y) f = f x y
fst (x, y) = x
snd (x, y) = y
uncurry f (x, y) = f x y
first f (x, y) = (f x, y)
second f (x, y) = (x, f y)
not a = if a then False else True
x /= y = not $ x == y
(.) f g x = f (g x)
(||) f g = if f then True else g
(&&) f g = if f then g else False
flst xs n c = case xs of [] -> n; h:t -> c h t
instance Eq a => Eq [a] where
  xs == ys = case xs of
    [] -> case ys of
      [] -> True
      _ -> False
    x:xt -> case ys of
      [] -> False
      y:yt -> x == y && xt == yt
take 0 xs = []
take _ [] = []
take n (h:t) = h : take (n - 1) t
drop n xs     | n <= 0 =  xs
drop _ []              =  []
drop n (_:xs)          =  drop (n-1) xs
splitAt n xs = (take n xs, drop n xs)
maybe n j m = case m of Nothing -> n; Just x -> j x
instance Functor Maybe where fmap f = maybe Nothing (Just . f)
instance Applicative Maybe where pure = Just ; mf <*> mx = maybe Nothing (\f -> maybe Nothing (Just . f) mx) mf
instance Monad Maybe where return = Just ; mf >>= mg = maybe Nothing mg mf
instance Alternative Maybe where empty = Nothing ; x <|> y = maybe y Just x
foldr c n l = flst l n (\h t -> c h(foldr c n t))
length = foldr (\_ n -> n + 1) 0
mapM f = foldr (\a rest -> liftA2 (:) (f a) rest) (pure [])
mapM_ f = foldr ((>>) . f) (pure ())
foldM f z0 xs = foldr (\x k z -> f z x >>= k) pure xs z0
instance Applicative IO where pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y)
instance Monad IO where return = ioPure ; (>>=) = ioBind
instance Functor IO where fmap f x = ioPure f <*> x
putStr = mapM_ putChar
getContents = getChar >>= \n -> if 0 <= n then (chr n:) <$> getContents else pure []
interact f = getContents >>= putStr . f
error s = unsafePerformIO $ putStr s >> putChar '\n' >> exitSuccess
undefined = error "undefined"
foldr1 c l@(h:t) = maybe undefined id $ foldr (\x m -> Just $ maybe x (c x) m) Nothing l
foldl f a bs = foldr (\b g x -> g (f x b)) (\x -> x) bs a
foldl1 f (h:t) = foldl f h t
reverse = foldl (flip (:)) []
dropWhile _ [] = []
dropWhile p xs@(x:xt)
  | p x       = dropWhile p xt
  | True = xs
elem k xs = foldr (\x t -> x == k || t) False xs
find f xs = foldr (\x t -> if f x then Just x else t) Nothing xs
(++) = flip (foldr (:))
concat = foldr (++) []
map = flip (foldr . ((:) .)) []
instance Functor [] where fmap = map
instance Applicative [] where pure = (:[]); f <*> x = concatMap (<$> x) f
instance Monad [] where return = (:[]); (>>=) = flip concatMap
concatMap = (concat .) . map
lookup s = foldr (\(k, v) t -> if s == k then Just v else t) Nothing
filter f = foldr (\x xs -> if f x then x:xs else xs) []
union xs ys = foldr (\y acc -> (if elem y acc then id else (y:)) acc) xs ys
intersect xs ys = filter (\x -> maybe False (\_ -> True) $ find (x ==) ys) xs
last xs = flst xs undefined last' where last' x xt = flst xt x \y yt -> last' y yt
init (x:xt) = flst xt [] \_ _ -> x : init xt
intercalate sep xs = flst xs [] \x xt -> x ++ concatMap (sep ++) xt
intersperse sep xs = flst xs [] \x xt -> x : foldr ($) [] (((sep:) .) . (:) <$> xt)
all f = foldr (&&) True . map f
any f = foldr (||) False . map f
zipWith f xs ys = flst xs [] $ \x xt -> flst ys [] $ \y yt -> f x y : zipWith f xt yt
zip = zipWith (,)
data State s a = State (s -> (a, s))
runState (State f) = f
instance Functor (State s) where fmap f = \(State h) -> State (first f . h)
instance Applicative (State s) where
  pure a = State (a,)
  (State f) <*> (State x) = State \s -> fpair (f s) \g s' -> first g $ x s'
instance Monad (State s) where
  return a = State (a,)
  (State h) >>= f = State $ uncurry (runState . f) . h
evalState m s = fst $ runState m s
get = State \s -> (s, s)
put n = State \s -> ((), n)
either l r e = case e of Left x -> l x; Right x -> r x
instance Functor (Either a) where fmap f e = either Left (Right . f) e
instance Applicative (Either a) where
  pure = Right
  ef <*> ex = case ef of
    Left s -> Left s
    Right f -> either Left (Right . f) ex
instance Monad (Either a) where
  return = Right
  ex >>= f = either Left f ex
iterate f x = x : iterate f (f x)
takeWhile _ [] = []
takeWhile p xs@(x:xt)
  | p x  = x : takeWhile p xt
  | True = []
class Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
asum = foldr (<|>) empty
(*>) = liftA2 \x y -> y
(<*) = liftA2 \x y -> x
many p = liftA2 (:) p (many p) <|> pure []
some p = liftA2 (:) p (many p)
sepBy1 p sep = liftA2 (:) p (many (sep *> p))
sepBy p sep = sepBy1 p sep <|> pure []
between x y p = x *> (p <* y)
class Enum a where
  succ           :: a -> a
  pred           :: a -> a
  toEnum         :: Int -> a
  fromEnum       :: a -> Int
  enumFrom       :: a -> [a]
  enumFrom = iterate succ
  enumFromTo     :: a -> a -> [a]
instance Enum Int where
  succ = (+1)
  pred = (+(0-1))
  toEnum = id
  fromEnum = id
  enumFromTo lo hi = takeWhile (<= hi) $ enumFrom lo
instance Enum Char where
  succ = chr . (+1) . ord
  pred = chr . (+(0-1)) . ord
  toEnum = chr
  fromEnum = ord
  enumFromTo lo hi = takeWhile (<= hi) $ enumFrom lo

-- Map.
data Map k a = Tip | Bin Int k a (Map k a) (Map k a)
size m = case m of Tip -> 0 ; Bin sz _ _ _ _ -> sz
node k x l r = Bin (1 + size l + size r) k x l r
singleton k x = Bin 1 k x Tip Tip
singleL k x l (Bin _ rk rkx rl rr) = node rk rkx (node k x l rl) rr
doubleL k x l (Bin _ rk rkx (Bin _ rlk rlkx rll rlr) rr) =
  node rlk rlkx (node k x l rll) (node rk rkx rlr rr)
singleR k x (Bin _ lk lkx ll lr) r = node lk lkx ll (node k x lr r)
doubleR k x (Bin _ lk lkx ll (Bin _ lrk lrkx lrl lrr)) r =
  node lrk lrkx (node lk lkx ll lrl) (node k x lrr r)
balance k x l r = f k x l r where
  f | size l + size r <= 1 = node
    | 5 * size l + 3 <= 2 * size r = case r of
      Tip -> node
      Bin sz _ _ rl rr -> if 2 * size rl + 1 <= 3 * size rr
        then singleL
        else doubleL
    | 5 * size r + 3 <= 2 * size l = case l of
      Tip -> node
      Bin sz _ _ ll lr -> if 2 * size lr + 1 <= 3 * size ll
        then singleR
        else doubleR
    | True = node
insert kx x t = case t of
  Tip -> singleton kx x
  Bin sz ky y l r -> case compare kx ky of
    LT -> balance ky y (insert kx x l) r
    GT -> balance ky y l (insert kx x r)
    EQ -> Bin sz kx x l r
insertWith f kx x t = case t of
  Tip -> singleton kx x
  Bin sy ky y l r -> case compare kx ky of
    LT -> balance ky y (insertWith f kx x l) r
    GT -> balance ky y l (insertWith f kx x r)
    EQ -> Bin sy kx (f x y) l r
mlookup kx t = case t of
  Tip -> Nothing
  Bin _ ky y l r -> case compare kx ky of
    LT -> mlookup kx l
    GT -> mlookup kx r
    EQ -> Just y
fromList = foldl (\t (k, x) -> insert k x t) Tip
member k t = maybe False (const True) $ mlookup k t
t ! k = maybe undefined id $ mlookup k t

foldrWithKey f = go where
  go z t = case t of
    Tip -> z
    Bin _ kx x l r -> go (f kx x (go z r)) l

putStrLn s = putStr s >> putChar '\n'

-- Program starts here

queens n = queens' n n

queens' n 0 = [[]]
queens' n k = [x:xs | xs <- queens' n (k - 1), x <- [1..n], isSafeColumn x xs, isSafeDiagonal x xs]

isSafeColumn x xs = not $ elem x xs

isSafeDiagonal x xs = all (\(a, b) -> abs(x - a) /= b) $ zip xs [1..]

abs :: Int -> Int
abs n = if 0 <= n then n else 0 - n

showLine n k = replicate (k - 1) '.' ++ "X" ++ replicate (n - k) '.'
showSolution s = (mapM_ putStrLn [showLine (length s) k | k <- s]) >> putStrLn ""

replicate 0 y = [  ]
replicate x y = y : replicate (x-1) y

main = mapM_ showSolution $ queens 8

