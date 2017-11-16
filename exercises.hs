import Data.List
import Data.Char 
-- ex 1
subtotal::Num a => [a] -> [a]
subtotal [] = []
subtotal (x:xs) = x:[x+e | e<-subtotal xs]

-- ex 2
histogram :: Int -> [Int] -> [Int]
histogram 0 xs = []
histogram x [] = []
histogram x xs = [length [e | e<-xs, e<a+x, e>=a] | a<-[0, x..maximum xs]]

-- ex 3
meetsOffer :: String -> Int -> Bool
meetsOffer "" a = if a<=0 then True else False 
meetsOffer (x:xs) a   | x == 'E' = meetsOffer xs (a-16)
                      | x == 'D' = meetsOffer xs (a-24)
                      | x == 'C' = meetsOffer xs (a-32)
                      | x == 'B' = meetsOffer xs (a-40)
                      | x == 'A' = meetsOffer xs (a-48)
                      | x == '*' = meetsOffer xs (a-8)
--ex 4
data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted
  deriving Show
sortType :: (Ord a) => [a] -> TypeOfSort
sortType [] = NotSorted
sortType [x] = NotSorted
sortType xs  | asc xs = Ascending
             | con xs = Constant
             | des xs = Descending
             | nonD xs = NonDescending
             | nonA xs = NonAscending
             | otherwise = NotSorted
  where
    asc [x] = True
    asc (x:y:xs) = x<y && asc (y:xs)
    con [x] = True
    con (x:y:xs) = x==y && con (y:xs)
    des [x] = True
    des (x:y:xs) = x>y && des (y:xs)
    nonA [x] = True
    nonA (x:y:xs) = x>=y && nonA (y:xs)
    nonD [x] = True
    nonD (x:y:xs) = x<=y && nonD(y:xs)

--ex 5
rpcalc :: String -> Int
rpcalc "" = 0
rpcalc (n:m:xs) = head (foldingFunction [digitToInt m, digitToInt n] xs)
  where
    foldingFunction stack [] = stack 
    foldingFunction (n:m:stack) (x:xs)  | x=='+'  = foldingFunction((n+m):stack) xs
                                        | x=='*'  = foldingFunction ((n*m):stack) xs
                                        | x=='-'  = foldingFunction ((m-n):stack) xs
                                        | x=='/'  = foldingFunction ((m `div` n):stack) xs
                                        | x=='^'  = foldingFunction ((m ^ n):stack) xs
                                        | otherwise =foldingFunction (digitToInt x:n:m:stack) xs
-- --ex 6
neighbours :: (Floating a, Ord a) => Int -> (a,a) -> [(a,a)] -> [(a,a)]
neighbours k p xs = [snd n | n<-take k sort]
  where
    sort = sortBy (comparing) neighbour
    distance c b = sqrt $ (fst c - fst b)^2 + (snd c - snd b)^2
    neighbour = [(distance p n, n) | n<-xs]
    comparing x y | fst x > fst y = GT
                  | fst x <= fst y = LT
                  | otherwise = EQ
    
--ex 7
data SearchTree = Node SearchTree Int SearchTree | Leaf Int
  deriving Show
balanced :: SearchTree -> Bool
balanced (Leaf _) = True
balanced (Node a b c) = if value  a<b && b<value c && abs(difference a c) <=1 && balanced a && balanced c then True else False
  where 
    value (Leaf a) = a
    value (Node a b c) = b
    difference a b = level a - level b
    level (Leaf _) = 1
    level (Node a b c) = (max(level a) (level c)) + 1
--ex 8
newtonRootSequence :: Double -> [Double]
newtonRootSequence d = 1 : next 1 d
  where
    next i d = x : next x d
      where
        x = (i+d/i)/2 

newtonRoot :: Double -> Double -> Double
newtonRoot d epsilon = get epsilon (newtonRootSequence d)
  where
    get epsilon (a:b:xs) = if abs (b-a) < epsilon then b else get epsilon (b:xs)

--ex 9
hyperOperator::Int -> Int -> Int -> Int

hyperOperator n a b  | n==0 = b+1
                     | n==1 && b==0 = a
                     | n==2 && b==0 = 0
                     | n>=3 && b==0 = 1
                     | otherwise = hyperOperator (n-1) a (hyperOperator n a (b-1))
--ex10
encode :: String -> [Int]
encode "" = []
encode(x:xs) = ninthBit ++ encode xs
  where
    uniCode = [if a == True then 1 else 0 | a <- map (toBit (ord x)) [7,6..0]]
    toBit x y = if (baseTwo x)!!y == 1 then True else False
    baseTwo n = [n `mod` 2] ++ baseTwo (n `div` 2)
    ninthBit = uniCode ++ [if length(filter (==1) uniCode) `mod` 2 == 1 then 1 else 0]
--ex11
decode :: [Int] -> String
decode [] = []
decode xs = if length xs `mod` 9 == 0 && check xs then
  [chr $ foldr str 0 (zip code [7,6..0])] ++ decode (drop 9 xs)
  else ""
  where
      check :: [Int] -> Bool 
      check [] = True
      check xs = if length(filter (==1) (take 9 xs)) `mod` 2 == 1 then False else check (drop 9 xs)
      code = take 8 $ take 9 xs
      str :: (Int,Int) -> Int -> Int
      str a b  = fst a*2 ^ snd a+b
--ex 12
makeChange :: (Num a, Eq a, Ord a) => a -> [a] -> [Int]
makeChange 0 xs = take (length xs) (repeat 0)
makeChange m xs = if m>0 then choose $ map f xs else take (length xs) (repeat (-1))
  where
    choose :: [[Int]] -> [Int]
    choose xs = minimumBy (cmp) xs
    cmp x y | pozSum x > pozSum y = GT
        | pozSum x == pozSum y = EQ
        | otherwise = LT
    --workaround the no solution case
    pozSum xs = if sum xs>=0 then sum xs else maxBound :: Int
    f x = incr (poz x xs) (makeChange (m-x) xs)
    poz y (x:xs) = if x==y then 0 else 1+poz y xs
    --dont increse lists of -1
    incr n xs = if length xs == length (filter (==(-1)) xs) then xs else
        take (n) xs ++ [xs!!n+1] ++ drop (n+1) xs 
--ex 13
goodsteinSequence :: (Int, [Int]) -> [(Int, [Int])]
goodsteinSequence (n, []) = [(n, [])]
goodsteinSequence (n, xs) = [(n, xs)] ++ goodsteinSequence ((n+1), (new ((old (n+1) 0 xs)-1)) (n+1))
  where
      old n b [] = 0
      old n b (x:xs) = x*(n^b) + old n (b+1) xs
      new 0 _ = []
      new n x = [n `mod` x] ++ new (n `div` x) x
--ex 14
--All the function except the isSat function are from the Hutton's textbook on Haskell
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop

type Assoc k v = [(k,v)]
fnd :: Eq k => k -> Assoc k v -> v
fnd k t = head [v | (k1,v)<-t , k==k1]

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x ) = fnd x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char ]
vars (Const _) = [ ]
vars (Var x ) = [x ]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [ [Bool ] ]
bools 0 = [[ ]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)

rmdups :: Eq a => [a ] -> [a ]
rmdups [] = []
rmdups (x:xs) = x:rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

isSat :: Prop -> Subst
isSat a = solution [(eval s a ,s) | s <- substs a]
  where 
    solution [] = []
    solution (x:xs) = if fst x then snd x else solution xs
--ex15
isCantorPair :: Int -> Bool
isCantorPair n = if xsum+ysum==y then True else False
  where
    cantor n = ((a-n+b), (n-b))
      where
        a = floor ((sqrt (fromIntegral (8*n)+1)-1)/2)
        b = (a^2+a) `div` 2
    x = fst $ cantor n
    y = snd $ cantor n
    xsum = fst $ cantor x
    ysum = snd $ cantor x


