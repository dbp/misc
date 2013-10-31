module Misc where

import Data.Ord (comparing)
import Data.List (sortBy)
import Debug.Trace (trace)
import Data.Maybe (isJust, isNothing, listToMaybe)
import Control.Monad (join)
import Control.Applicative ((<$>), (<*>))

powMod :: Integer -> Integer -> Integer -> Integer
powMod g n m = powMod' g n
  where powMod' t 0 = 1
        powMod' t 1 = t
        powMod' t n = powMod' ((t*g) `mod` m) (n-1)

ordMod :: Integer -> Integer -> Integer
ordMod n m = ordMod' 1 n
  where ordMod' k 1 = k
        ordMod' k acc = ordMod' (k+1) (acc * n `mod` m)

invMod :: Integer -> Integer -> Integer
invMod n m = powMod n (m - 2) m

-- gotten from rosetta code
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)

-- Given a and m, return Just x such that ax = 1 mod m.  If there is no such x
-- return Nothing.
invModGen a m = let (i, _, g) = gcdExt a m
             in if g == 1 then Just (mkPos i) else Nothing
  where mkPos x = if x < 0 then x + m else x


babyGiant :: Integer -> Integer -> Integer -> Integer
babyGiant g h m' = construct $ findMatch $ sortBy (comparing (\(_,_,x) -> x))
                            $ (map (\x -> (0,fst x, snd x)) bList) ++
                              (map (\x -> (1,fst x, snd x)) gList)
  where
    m = ordMod g m'
    n = 1 + floor (sqrt $ fromIntegral m)
    buildBaby c g (-1) = []
    buildBaby c g n' = (n - n', c):(buildBaby (c*g `mod` m') g (n'-1))
    bList = buildBaby 1 g n
    buildGiant c gn 0 = []
    buildGiant c gn n' = (n - n', c):(buildGiant (c*gn `mod` m') gn (n'-1))
    gList = buildGiant h (invMod (snd $ last bList) m') n
    findMatch [] = Nothing
    findMatch (_:[]) = Nothing
    findMatch ((0,i,x):(1,j,y):_) | x == y = Just (i,j)
    findMatch ((1,i,x):(0,j,y):_) | x == y = Just (i,j)
    findMatch (_:xs) = findMatch xs
    construct Nothing = -1
    construct (Just (i,j)) = (i + j * n) `mod` m

factors :: Integer -> [Integer]
factors n = filter (\x -> n `mod` x == 0) [1..n]

primeFactors :: Integer -> [Integer]
primeFactors n = (pfs $ tail $ factors n)
  where pfs [] = []
        pfs (x:xs) = x:(pfs $ filter (\y -> (y `mod` x) /= 0) xs)

units :: Integer -> [Integer]
units n = filter (\x -> any (\y -> x * y `mod` n == 1) [1..(n-1)]) [1..(n-1)]


invModBrute :: Integer -> Integer -> [(Integer,Integer)]
invModBrute n m = filter (\(_,b) -> b == 1) $ map (\x -> (x, n * x `mod` m)) [1..m-1]

data CompositeT = Composite | PossiblyPrime deriving Show
eq1n :: Integer -> Integer -> Bool
eq1n n x = x == 1 || x == n
millerRabin :: Integer -> Integer -> CompositeT
millerRabin n _ | n `mod` 2 == 0 = Composite
millerRabin n a | not (eq1n n (gcd a n)) = Composite
millerRabin n a = if aq == 1 then PossiblyPrime else mr k aq
  where
    div2 l x = if (x `mod` 2) == 0 then div2 (l+1) (x `div` 2) else (x,l)
    (q,k) = div2 0 (n-1)
    aq = (powMod a q n)
    mr 0 _ = Composite
    mr i x = if x == n-1 then PossiblyPrime else if x == 1 then Composite else mr (i-1) (x*x `mod` n)


-- alphabetSize :: [Integer] -> Integer
alphabetSize l = head $ filter ((<= 1) . fst) $ map (\d -> (sum $ map ((1/) . (d^)) l, d)) [1..]

tracePass :: Show a => a -> a
tracePass a = trace (show a) a

pollard :: Integer -> Integer -> Maybe Integer
pollard n b = join $ listToMaybe $ filter isJust $ map checkGcd $ map prettyPass $ map inner $ snd $ foldl buildPows (2, []) [2..b]
  where
    buildPows (a,l) j = let newa = powMod a j n in (newa, l ++ [(j, newa)])
    inner (j,x) = let d = gcd (x - 1) n in (j, x-1, d)
    checkGcd (_,_,d) = if (not (eq1n n d)) then Just d else Nothing
    prettyPass inp@(j, aj, gc) = trace ("2^" ++ (show j) ++ "! - 1 = " ++ (show aj) ++ " (mod " ++ (show n) ++ "), gcd = " ++ (show gc)) inp


-- like takeWhile, but also includes first matched element
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []          = []
takeUntil f (x:_) | f x = [x]
takeUntil f (x:xs)      = x : (takeUntil f xs)

diffSquares :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
diffSquares n k binit = join $ listToMaybe $ filter isJust $ map snd $
                        tracePass $ takeUntil (isJust . snd) $
                        map isSquare $ map (\b -> (b, k*n + b^2)) [binit..]
  where isSquare (b, x) = let possibleRoot = floor $ sqrt $ fromIntegral x in
                             if possibleRoot^2 == x then ((b,x), Just (b, possibleRoot)) else ((b,x), Nothing)

bSmooth :: Integer -> Integer -> Integer
bSmooth x b = fromIntegral $ length $ filter (<= b) $
              map (maximum.snd) $ tracePass $ map (\x -> (x, primeFactors x)) [2..x]

stupidPrime :: Integer -> Bool
stupidPrime n = null $ filter (== 0) $ map (\x -> n `mod` x) [2..n-1]


isQuadRes :: Integer -> Integer -> Maybe Integer
isQuadRes n p = listToMaybe $ filter (\x -> x*x `mod` p == n) [1..p-1]

isCubRes :: Integer -> Integer -> Maybe Integer
isCubRes n p = listToMaybe $ filter (\x -> x*x*x `mod` p == n) [1..p-1]


findCubicCounterExample :: Maybe (Integer, (Integer, Integer, Integer))
findCubicCounterExample = join $ listToMaybe $
                          filter isJust $
                          map (\p -> (,) <$> (Just p) <*>
                                     (listToMaybe $ map fst $ filter (isNothing.snd) $ tracePass
                                     [((a,b,a*b `mod` p), trip
                                                (isCubRes a p)
                                                (isCubRes b p)
                                                (isCubRes (a*b `mod` p) p))
                                     | a <- [2..p-1], b <- [2..p-1]])) $
                          filter stupidPrime [2..]
  where trip Nothing Nothing Nothing = Nothing
        trip a b c = Just (a,b,c)
