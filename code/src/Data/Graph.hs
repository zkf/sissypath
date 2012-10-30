module Data.Graph
( randomGraph
, randomGraph'
, graphFromList
, Graph)
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
         theNodes' = V.replicate size 0.0
     theEdges <- randomEdges size (round $ fromIntegral size * (3 :: Double))
     let theNodes = Nodes $ dangerzone theNodes' theEdges
     return $ Graph theNodes theEdges
  where dangerzone :: Vector Double -> Edges -> Vector Double
        dangerzone theNodes (Edges theEdges) =
           let neighbourTree = [0] : nlevels [0] 0
               newNodes =
                   concat
                   $ zipWith (\ns p -> zip ns (repeat p))
                             neighbourTree [1.0, 0.5]
           in theNodes // newNodes
          where nlevels :: [Int] -> Int -> [[Int]]
                nlevels v i = let myNs =  (theEdges ! i) \\ v
                                  v'   = v ++ myNs
                              in myNs : concatMap (nlevels v') myNs

randomGraph' :: MonadRandom m => Int -> m (Graph (Char, (Double, Double)))
randomGraph' size =
  do thePoints <- take size `liftM` points
     let theNodes = Nodes . V.fromList $ take size (zip ['a'..] thePoints)
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
    in Graph (Nodes theNodes) theEdges

