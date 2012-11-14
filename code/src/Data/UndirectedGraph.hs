module Data.UndirectedGraph
( randomGraph
, randomGraph'
, graphFromList
, Graph
, edges
, nodes
, cost
, Path)
where

import Data.UndirectedGraph.Internal
import Control.Monad
import Control.Monad.LazyRandom
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import Data.List (sort, nub, deleteFirstsBy)
import Data.Set ((\\), Set, union, singleton)
import qualified Data.Set as S

randomGraph :: MonadRandom m => Int -> Int -> Int -> m (Graph Double)
randomGraph size numEdges dangerLevel =
  do -- nodesList <- take size `liftM` getRandomRs (0.0, 1.0)
     let -- nodes = Nodes $ V.fromList nodesList
         theNodes' = zip [0..] $ replicate size 0.0
     theEdges <- randomEdges size numEdges
     pointZero <- getRandomR (0, size - 1)
     let theNodes = IM.fromList $ insertDangerZone pointZero theNodes' theEdges
     return $ Graph theNodes theEdges
  where insertDangerZone :: Int -> [(Int, Double)] -> Edges -> [(Int, Double)]
        insertDangerZone pointZero theNodes theEdges =
           let newNodes = concat
                   $ zipWith (\ns p -> zip (S.toList ns) (repeat p))
                             neighbourTree (replicate dangerLevel 1.0)
               neighbourTree = singleton pointZero : nlevels (singleton pointZero) pointZero
           in theNodes // newNodes
         where nlevels :: Set Int -> Int -> [Set Int]
               nlevels v i = let myNs =  (theEdges ! i) \\ v
                                 v'   = v `union` myNs
                             in myNs : concatMap (nlevels v') (S.toList myNs)
               as // bs = let as' = deleteFirstsBy (\a b -> fst a == fst b) as bs
                            in as' ++ bs

randomGraph' :: MonadRandom m => Int -> m (Graph (Char, (Double, Double)))
randomGraph' size =
  do thePoints <- take size `liftM` points
     let theNodes = IM.fromList $ zip [1..] $ take size (zip ['a'..] thePoints)
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
    let theNodeIndices = nub . uncurry (++) $ unzip rels
        theValues = take len vals
        theNodes  = IM.fromList $ zip theNodeIndices theValues
        len = length theNodeIndices
        theEdges = neighboursFromList rels len
    in Graph (theNodes) theEdges

