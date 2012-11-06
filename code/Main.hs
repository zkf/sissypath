module Main where
import qualified Data.Set as S
import Data.UndirectedGraph
import Data.UndirectedGraph.Path.Random
import Data.UndirectedGraph.Path.Bruteforce
import Data.UndirectedGraph.Path.Dijkstra
import Data.UndirectedGraph.Path.UCB1
-- import Graphics.Gloss hiding (Vector)
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import Control.Monad
import Data.List


main :: IO ()
main =
  do g <- randomGraph 15 30 4
     let p1 = bruteForceOptimalPath 0 7 g
     p2 <- randomPath 0 7 g
     p3 <- randomPathWithMemory 0 7 g
     let [c1, c2, c3] = map (cost g) [p1, p2, p3]
     print g
     putStrLn $ "bruteforce:   " ++ show c1 ++ " " ++ show p1
           ++ "\nrandom:       " ++ show c2 ++ " " ++ show p2
           ++ "\nrandom w/mem: " ++ show c3 ++ " " ++ show p3
  -- do g <- randomGraph 15
  --    let p =  bruteForceOptimalPath 0 7 g
  --        c = cost g p
  --    printGraph g
  --    putStrLn $ "path: " ++ show p ++ " cost: " ++ show c
  --    displayGraph g
     --print . last $ paths 0 1 g

dijkstraCost start goal graph = cost graph $ dijkstra start goal graph

avgRndCost fun start goal graph n =
  do res <- replicateM n $ fun start goal graph
     return $ sum res / fromIntegral n

avgBanditCost start goal graph n =
  do iters <- replicateM n $ liftM (take 1 . map (cost graph)) $ banditsPath start goal graph
     let iters' = transpose iters
         avgs = map (/ fromIntegral n) $ map sum iters'
     return avgs

-- displayGraph :: Graph (Char, (Double, Double)) -> IO ()
-- displayGraph g = display window black (renderGraph dims g)
--   where window = InWindow "graph" dims (0, 0)
--         dims   = (800, 600)

-- renderGraph :: (Int, Int) -> Graph (Char, (Double, Double)) -> Picture
-- renderGraph (width, height) graph =
--     Pictures [nodesPic, edgesPic]
--   where
--         widthF = fromIntegral width
--         heightF = fromIntegral height
--         ns = nodes graph
--         es = edges graph
--         nsF = IM.map (\(v, c) -> (v, toFracs c)) ns
--         toFracs (a, b) = (realToFrac a, realToFrac b)
--         nodesPic = Color white . Pictures . map renderNode $ IM.elems nsF
--         edgesPic = Color white . Pictures . IM.elems
                   -- $ IM.mapWithKey renderEdges es
        -- renderNode (_,(x,y)) = Translate ((x - 0.5) * widthF)
                   --                       ((y - 0.5) * heightF)
                   --             $ circleSolid 5
        -- renderEdges i fs = Pictures . map (renderEdge i) $ S.toList fs
        -- renderEdge i e   =
            -- let (_,(x0, y0)) = nsF ! i
                -- (_,(x1, y1)) = nsF ! e
                -- x0' = (x0 - 0.5) * widthF
                -- y0' = (y0 - 0.5) * heightF
                -- x1' = (x1 - 0.5) * widthF
                -- y1' = (y1 - 0.5) * heightF
            -- in Line [(x0', y0'), (x1', y1')]

