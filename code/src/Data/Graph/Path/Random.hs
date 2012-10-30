module Data.Graph.Path.Random where

import Data.Graph.Internal
import Control.Monad.Random
import Control.Monad
import Data.Vector ((!))

-- | Randomly find a path from start to goal through the given graph.
randomPath :: MonadRandom m => Int -> Int -> Graph Double -> m [Int]
randomPath start goal (Graph ns es) = liftM (start:) $ go start
  where go current
          | current == goal = return []
          | otherwise = do let myNeighbours = es ! current
                           next <- fromList $ zip myNeighbours [1, 1..]
                           liftM (next:) $ go next

-- | Randomly find a path from start to goal through the given graph. The path
-- will not have any repeated vertices.
randomPathWithMemory :: MonadRandom m => Int -> Int -> Graph Double -> m [Int]
randomPathWithMemory start goal (Graph ns es) = go [start] start
  where go :: MonadRandom m => [Int] -> Int -> m [Int]
        go visited current
          | current == goal = return [current]
          | otherwise =
              do let myUnvisitedNeighbours =
                         filter (`notElem` visited) $ es ! current
                 if null myUnvisitedNeighbours
                    then return []
                    else do next <- fromList $ zip myUnvisitedNeighbours [1, 1..]
                            further <- go (next : visited) next
                            if null further
                               then go (next : visited) current
                               else return (current : further)

