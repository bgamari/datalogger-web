module Average (Average, average, getAverage) where

import Data.Monoid
 
data Average a = Avg Int a
               deriving (Show)
               
instance (Monoid a, Num a) => Monoid (Average a) where
     mempty = Avg 0 0
     Avg n a `mappend` Avg m b = Avg (n+m) (a+b)
     
getAverage :: Fractional a => Average a -> a
getAverage (Avg n s) = s / realToFrac n

average :: a -> Average a
average = Avg 1
