module Data.UndirectedGraph.Path.Random where

import Data.UndirectedGraph.Internal
import Control.Monad.Random
import Control.Monad
import Data.Vector ((!))
import Data.Set (Set, member, insert, (\\), singleton)
import qualified Data.Set as S

-- | Randomly find a path from start to goal through the given graph.
randomPath :: MonadRandom m => Int -> Int -> Graph Double -> m [Int]
randomPath start goal (Graph ns es) = liftM (start:) $ go start
  where go current
          | current == goal = return []
          | otherwise = do let myNeighbours = es ! current
                           next <- fromList $ zip (S.toList myNeighbours) [1, 1..]
                           liftM (next:) $ go next

-- | Randomly find a path from start to goal through the given graph. The path
-- will not have any repeated vertices.
randomPathWithMemory :: MonadRandom m => Int -> Int -> Graph Double -> m [Int]
randomPathWithMemory start goal (Graph ns es) = go (singleton start) start
  where go :: MonadRandom m => Set Int -> Int -> m [Int]
        go visited current
          | current == goal = return [current]
          | otherwise =
              do let myUnvisitedNeighbours = es ! current \\ visited
                 if S.null myUnvisitedNeighbours
                    then return []
                    else do next <- fromList $ zip (S.toList myUnvisitedNeighbours) [1, 1..]
                            let unvisited' = next `insert` visited
                            further <- go unvisited' next
                            if null further
                               then go unvisited' current
                               else return (current : further)

