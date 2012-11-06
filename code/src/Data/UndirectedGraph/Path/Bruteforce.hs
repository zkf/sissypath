module Data.UndirectedGraph.Path.Bruteforce
where

import Data.UndirectedGraph.Internal hiding (null)
import qualified Data.UndirectedGraph.Internal as G
import Data.IntMap ((!))
import Data.Set (insert, singleton, (\\))
import qualified Data.Set as S
import Data.List hiding ((\\), insert)
import Data.Function (on)

-- | Determines the shortest path by enumerating all possible paths from a to
-- b and then finding their cost. If more than one path has the same cost, the
-- path with the fewest nodes is returned, ties broken arbitrarily.
bruteForceOptimalPath :: Int -> Int -> Graph Double -> [Int]
bruteForceOptimalPath a b graph =
    let allPaths = enumeratePaths a b graph
        myOrdering l1 l2 = let costOrder = (compare `on` cost graph) l1 l2
                     in if costOrder /= EQ
                            then costOrder
                            else (compare `on` length) l1 l2
    in minimumBy myOrdering allPaths

-- | Enumerate all paths from start to goal.
enumeratePaths :: Int -> Int -> Graph a -> [[Int]]
enumeratePaths _ _ g | G.null g = []
enumeratePaths start goal (Graph _ es) = go (singleton start) start
  where go visited current
          | current == goal = [[goal]]
          | otherwise =
              let myNeighbours = S.toList $ es ! current \\ visited
              in if null myNeighbours
                     then []
                     else let visited' = current `insert` visited
                              furtherPaths = concatMap (go visited') myNeighbours
                          in map (current :) $ filter (not . null) furtherPaths


