module Data.UndirectedGraph.Path.Dijkstra where
import Prelude hiding (null)
import Data.UndirectedGraph.Internal
import qualified Data.Vector as V
import Data.Vector ((//), Vector)
import Data.IntMap ((!))
import qualified Data.IntMap as IM
import qualified Data.Set as S
--import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.PSQueue as PQ
import Data.PSQueue (Binding(..), minView)
import Debug.Trace

-- | Perform a shortest-path search according to Dijkstra's algorithm.
dijkstra :: Int -> Int -> Graph Double -> [Int]
dijkstra start goal (Graph ns es) =
    let distances = V.replicate (IM.size ns) infinity // [(start, ns ! start)]
        unvisited = PQ.fromList $ zipWith (:->) [0::Int ..] (V.toList distances)
        previous  = V.replicate (IM.size ns) (-1)
        previous' = go unvisited previous
    in reverse $ shortestPath previous' goal
  where infinity = 1 / 0
        go unvisited previous
          | PQ.null unvisited = previous
          | otherwise =
              let Just ((current :-> tenDist), unvisited') = minView unvisited
                  neighbours = es ! current
                  (unvisited'', previous') =
                      S.foldr
                        (\n (unvs, prevs) ->
                          updateDist unvs prevs tenDist current n)
                        (unvisited', previous) neighbours
              in if current == goal then previous' else go unvisited'' previous'
        updateDist :: PQ.PSQ Int Double -> Vector Int -> Double -> Int -> Int -> (PQ.PSQ Int Double, Vector Int)
        updateDist unvisited previous tenDistCurrent current neighbour =
            let probNeighbour = ns ! neighbour
            in case PQ.lookup neighbour unvisited of
                    Nothing       -> (unvisited, previous)
                    Just tenDistNeighbour  ->
                         let tenDist' = 1 - (1 + tenDistCurrent) * (1 - probNeighbour)
                         in if tenDist' < tenDistNeighbour
                               then (PQ.insert neighbour tenDist' unvisited
                                    , previous // [(neighbour, current)])
                               else (unvisited, previous)

        shortestPath :: Vector Int -> Int -> [Int]
        shortestPath _ (-1) = []
        shortestPath previous n = n : shortestPath previous (previous V.! n)

shortestPathTree :: Int -> Graph Double -> [[(Int, Double)]]
shortestPathTree start g@(Graph ns _) = dijkstraTree (PQ.fromList [([(start, d)] :-> d)]) g
  where d = ns ! start

dijkstraTree :: PQ.PSQ [(Int, Double)] Double -> Graph Double -> [[(Int, Double)]]
dijkstraTree _ g | null g = []
dijkstraTree q g = p:dijkstraTree (q' `insertAll` expand p g) g'
  where Just ((p@((n, d):_) :-> _), q') = minView q
        (_, _, g') = view n g

insertAll :: Ord a => PQ.PSQ a Double -> [(a, Double)] -> PQ.PSQ a Double
insertAll q l = foldl (\q' (e, p) -> PQ.insert e p q') q l

expand :: [(Int, Double)] -> Graph Double -> [([(Int, Double)], Double)]
expand p@((n, d):_) (Graph ns es) = map (\v -> ((v, newVal v):p, newVal v)) myNs
  where myNs = traceIt "Edges" es $ S.toList $ es ! n
        newVal v = traceIt "Nodes" ns $ d + ns ! v

traceIt what s = trace $ "\n---" ++ what ++ ":\n " ++ show s ++ "\n----\n"

