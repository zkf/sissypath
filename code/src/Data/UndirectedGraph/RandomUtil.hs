module Data.UndirectedGraph.RandomUtil
    ( twoRandoms
    , randomPairs
    , withProbability
    ) where
import Control.Monad.Random
import Control.Monad
import Data.List (nub)

twoRandoms :: MonadRandom m => (Int, Int) -> m (Int, Int)
twoRandoms bnds = head `liftM` randomPairs 1 bnds

-- | randomPairs generates n unique pairs, where the elements are not equal.
randomPairs :: MonadRandom m => Int -> (Int, Int) -> m [(Int, Int)]
randomPairs n bnds
  | uncurry (>=) bnds  = error $ "invalid bounds: " ++ show bnds
  | maxPossiblePairs bnds < n = error $ "there are less than "
                                    ++ show n ++ " possible pairs in "
                                    ++ show bnds
  | otherwise =
    do [as, bs] <- replicateM 2 $ getRandomRs bnds
       let pairs = filter (uncurry (/=))
                   $ zipWith smallestFirst as bs
       return . take n $ nub pairs
  where smallestFirst a b = if a < b then (a, b) else (b, a)
        maxPossiblePairs (a, b) = let m = fromIntegral $ b - a
                                  in floor $ m * (m + 1) / (2::Float)

withProbability :: MonadRandom m => Float -> (b -> m b) -> b -> m b
withProbability p action arg =
        do yes <- decide
           if yes then action arg else return arg
        where decide = (< p) `liftM` getRandomR (0, 1::Float)

