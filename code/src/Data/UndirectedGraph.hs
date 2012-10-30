module Data.UndirectedGraph
( randomGraph
, randomGraph'
, graphFromList
, Graph
, edges
, nodes
, cost)
where

import Data.UndirectedGraph.Internal
import Control.Monad
import Control.Monad.Random
import qualified Data.Vector as V
import Data.Vector (Vector, (!), (//))
import Data.List ((\\), sort, nub)

randomGraph :: MonadRandom m => Int -> Int -> m (Graph Double)
randomGraph size numEdges =
  do -- nodesList <- take size `liftM` getRandomRs (0.0, 1.0)
     let -- nodes = Nodes $ V.fromList nodesList
         theNodes' = V.replicate size 0.0
     theEdges <- randomEdges size numEdges
     pointZero <- getRandomR (0, size - 1)
     let theNodes = insertDangerZone pointZero theNodes' theEdges
     return $ Graph theNodes theEdges
  where insertDangerZone :: Int -> Vector Double -> Edges -> Vector Double
        insertDangerZone pointZero theNodes theEdges =
           let neighbourTree = [pointZero] : nlevels [pointZero] pointZero
               newNodes =
                   concat
                   $ zipWith (\ns p -> zip ns (repeat p))
                             neighbourTree [1.0, 1.0]
           in theNodes // newNodes
          where nlevels :: [Int] -> Int -> [[Int]]
                nlevels v i = let myNs =  (theEdges ! i) \\ v
                                  v'   = v ++ myNs
                              in myNs : concatMap (nlevels v') myNs

randomGraph' :: MonadRandom m => Int -> m (Graph (Char, (Double, Double)))
randomGraph' size =
  do thePoints <- take size `liftM` points
     let theNodes = V.fromList $ take size (zip ['a'..] thePoints)
         neighbours =  map (findNeighbours $ zip [0..] thePoints) thePoints
         relations = concat $ zipWith (\ns i -> map (\n -> (i, n)) ns) neighbours [0..]
         theEdges = neighboursFromList relations size
     return $ Graph theNodes theEdges
  where dist (x1, y1) (x2, y2) = sqrt $ (x2 - x1)**2 + (y2 - y1)**2
        findNeighbours :: [(Int, (Double, Double))] -> (Double, Double) -> [Int]
        findNeighbours ps p = snd . unzip . take 3 . sort
                              . map (\(i, q) -> (dist p q, i))
                              $ filter (\(_, q) -> q /= p) ps
        points =
          do as <- getRandomRs (0, 1::Double)
             bs <- getRandomRs (0, 1::Double)
             return $ zip as bs

graphFromList :: [(Int, Int)] -> [a] -> Graph a
graphFromList rels vals =
    let theNodes = V.fromList
                   . flip take vals . length . nub . uncurry (++) $ unzip rels
        len = V.length theNodes
        theEdges = neighboursFromList rels len
    in Graph (theNodes) theEdges

