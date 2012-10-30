module Data.UndirectedGraph.Path.Dijkstra where

import Data.UndirectedGraph.Internal
import Data.List
import qualified Data.Vector as V
import Data.Vector ((!), (//))
import qualified Data.Set as S

-- | Perform a shortest-path search according to Dijkstra's algorithm.
dijkstra :: Int -> Int -> Graph Double -> [Int]
dijkstra start goal (Graph ns es) =
    let distances = V.replicate (V.length ns) infinity // [(start, ns ! start)]
        unvisited = S.fromList $ [0..V.length ns - 1]
        result = go unvisited distances start
    in undefined
  where infinity = 1 / 0
        go uvisited distances = undefined



