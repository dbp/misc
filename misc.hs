module Misc where

modPow :: Integer -> Integer -> Integer -> Integer
modPow g n m = modPow' g g n m
  where modPow' t g 0 m = 1
        modPow' t g 1 m = t
        modPow' t g n m = modPow' ((t*g) `mod` m) g (n-1) m