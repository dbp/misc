module Misc where

import Data.Ord (comparing)
import Data.List (sortBy)
import Debug.Trace (trace)

powMod :: Integer -> Integer -> Integer -> Integer
powMod g n m = powMod' g g n m
  where powMod' t g 0 m = 1
        powMod' t g 1 m = t
        powMod' t g n m = powMod' ((t*g) `mod` m) g (n-1) m

ordMod :: Integer -> Integer -> Integer
ordMod n m = ordMod' 1 n
  where ordMod' k 1 = k
        ordMod' k acc = ordMod' (k+1) (acc * n `mod` m)

invMod :: Integer -> Integer -> Integer
invMod n m = powMod n (m - 2) m

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