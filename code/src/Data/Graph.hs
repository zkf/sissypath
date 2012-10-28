module Data.Graph
( randomGraph
, randomGraph'
, graphFromList)
where

import Data.Graph.Internal
import Control.Monad
import Control.Monad.Random
import qualified Data.Vector as V
import Data.Vector (Vector, (!), (//))
import Data.List ((\\), sort, nub)

randomGraph :: MonadRandom m => Int -> m (Graph Double)
randomGraph size =
  do -- nodesList <- take size `liftM` getRandomRs (0.0, 1.0)
     let -- nodes = Nodes $ V.fromList nodesList
         nodes' = V.replicate size 0.0
     edges <- randomEdges size (round $ fromIntegral size * (3 :: Double))
     let nodes = Nodes $ dangerzone nodes' edges
     return . Graph $  (nodes, edges)
  where dangerzone :: Vector Double -> Edges -> Vector Double
        dangerzone nodes (Edges edges) =
           let neighbourTree = [0] : nlevels [0] 0
               newNodes =
                   concat
                   $ zipWith (\ns p -> zip ns (repeat p))
                             neighbourTree [1.0, 0.5]
           in nodes // newNodes
          where nlevels :: [Int] -> Int -> [[Int]]
                nlevels v i = let myNs =  (edges ! i) \\ v
                                  v'   = v ++ myNs
                              in myNs : concatMap (nlevels v') myNs

randomGraph' :: MonadRandom m => Int -> m (Graph (Char, (Double, Double)))
randomGraph' size =
  do thePoints <- take size `liftM` points
     let nodes = Nodes . V.fromList $ take size (zip ['a'..] thePoints)
         neighbours =  map (findNeighbours $ zip [0..] thePoints) thePoints
         relations = concat $ zipWith (\ns i -> map (\n -> (i, n)) ns) neighbours [0..]
         edges = neighboursFromList relations size
     return . Graph $ (nodes, edges)
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
    let nodes = flip take vals . length . nub . uncurry (++) $ unzip rels
        len = length nodes
        edges = neighboursFromList rels len
    in Graph (Nodes $ V.fromList nodes, edges)

