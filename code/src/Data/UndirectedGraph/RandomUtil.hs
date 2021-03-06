module Data.UndirectedGraph.RandomUtil
    ( twoRandoms
    , randomPairs
    , withProbability
    ) where
import Control.Monad.LazyRandom
import Control.Monad
import Data.List (nub)

twoRandoms :: MonadRandom m => (Int, Int) -> m (Int, Int)
twoRandoms bnds = head `liftM` randomPairs bnds

-- | randomPairs generates unique pairs where the elements are not equal.
randomPairs :: MonadRandom m => (Int, Int) -> m [(Int, Int)]
randomPairs bnds
  | uncurry (>=) bnds  = error $ "invalid bounds: " ++ show bnds
  | otherwise =
    do [as, bs] <- replicateM 2 $ getRandomRs bnds
       let pairs = filter (uncurry (/=))
                   $ zipWith smallestFirst as bs
       return $ nub pairs
  where smallestFirst a b = if a < b then (a, b) else (b, a)

withProbability :: MonadRandom m => Float -> (b -> m b) -> b -> m b
withProbability p action arg =
        do yes <- decide
           if yes then action arg else return arg
        where decide = (< p) `liftM` getRandomR (0, 1::Float)

