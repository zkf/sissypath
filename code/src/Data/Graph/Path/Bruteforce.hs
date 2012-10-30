module Data.Graph.Path.Bruteforce
where

import Data.Graph.Internal
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.List
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
enumeratePaths _ _ (Graph ns _)
  | V.null ns = []
enumeratePaths start goal (Graph _ es) =
    go [] start
  where go visited current
          | current == goal = [[goal]]
          | current `elem` visited = []
          | otherwise =
              let myNeighbours = if null visited
                                     then es ! current
                                     else nextNeighbours current (head visited)
              in if null myNeighbours then []
                     else let furtherPaths = concatMap (go (current:visited)) myNeighbours
                          in map (current :) $ filter (not . null) furtherPaths

        nextNeighbours me prev = filter (/= prev) $ es ! me

