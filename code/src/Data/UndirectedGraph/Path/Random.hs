module Data.UndirectedGraph.Path.Random where

import Data.UndirectedGraph.Internal hiding (null)
import Control.Monad.LazyRandom
import Control.Monad.Random (fromList)
import Control.Monad
import Data.IntMap ((!))
import Data.Set (Set, insert, (\\), singleton)
import qualified Data.Set as S

-- | Randomly find a path from start to goal through the given graph.
randomPath :: MonadRandom m => Int -> Int -> Graph Double -> m [Int]
randomPath start goal (Graph _ es) = liftM (start:) $ go start
  where go current
          | current == goal = return []
          | otherwise = do let myNeighbours = es ! current
                           nextNode <- fromList $ zip (S.toList myNeighbours) [1, 1..]
                           liftM (nextNode :) $ go nextNode

-- | Randomly find a path from start to goal through the given graph. The path
-- will not have any repeated vertices.
randomPathWithMemory :: MonadRandom m => Int -> Int -> Graph Double -> m [Int]
randomPathWithMemory start goal (Graph _ es) = go (singleton start) start
  where go :: MonadRandom m => Set Int -> Int -> m [Int]
        go visited current
          | current == goal = return [current]
          | otherwise =
              do let myUnvisitedNeighbours = es ! current \\ visited
                 if S.null myUnvisitedNeighbours
                    then return []
                    else do nextNode <- fromList $ zip (S.toList myUnvisitedNeighbours) [1, 1..]
                            let unvisited' = nextNode `insert` visited
                            further <- go unvisited' nextNode
                            if null further
                               then go unvisited' current
                               else return (current : further)

