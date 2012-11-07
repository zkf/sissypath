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
import Control.Monad.Random
import Data.List


main :: IO ()
main =
  do graph <- randomGraph 30 60 3
     gen <- newStdGen
     let reps = 100
         iters = 100
         res = evalRand (test graph 0 7 reps iters) gen
         ppRes = map pp res
     putStrLn (unlines . map unwords $ transpose ppRes)
     -- let p1 = bruteForceOptimalPath 0 7 g
     -- p2 <- randomPath 0 7 g
     -- p3 <- randomPathWithMemory 0 7 g
     -- let [c1, c2, c3] = map (cost g) [p1, p2, p3]
     -- print g
     -- putStrLn $ "bruteforce:   " ++ show c1 ++ " " ++ show p1
     --       ++ "\nrandom:       " ++ show c2 ++ " " ++ show p2
     --       ++ "\nrandom w/mem: " ++ show c3 ++ " " ++ show p3
  -- do g <- randomGraph 15
  --    let p =  bruteForceOptimalPath 0 7 g
  --        c = cost g p
  --    printGraph g
  --    putStrLn $ "path: " ++ show p ++ " cost: " ++ show c
  --    displayGraph g
     --print . last $ paths 0 1 g
     --
     --

pp (label, d) = label : map show d

test graph start goal reps iters = do
    banditRes <- repeatedBanditPaths start goal graph reps iters
    let banditAvgs = avgBanditCost banditRes graph reps
        dijkstraAvg = dijkstraCost start goal graph
    rndAvg <- replicateM iters $ avgRndCost randomPath start goal graph reps
    rndMemAvg <- replicateM iters $ avgRndCost randomPathWithMemory start goal graph reps
    return [("dijkstra", replicate iters dijkstraAvg)
           ,("random",   rndAvg)
           ,("random w/mem", rndMemAvg)
           ,("bandits"     , banditAvgs)
           ]

dijkstraCost start goal graph = cost graph $ dijkstra start goal graph

avgRndCost :: MonadRandom m =>
    (Int -> Int -> Graph Double -> m [Int]) -> Int -> Int -> Graph Double -> Int -> m Double
avgRndCost fun start goal graph n =
  do res <- replicateM n $ liftM (cost graph) $ fun start goal graph
     return $ sum res / fromIntegral n

avgBanditCost repeatedIteratedPaths graph reps =
  avgIterCosts' graph repeatedIteratedPaths

repeatedBanditPaths start goal graph reps iters =
  replicateM reps $ banditsPath start goal graph iters

avgBanditCost' :: MonadRandom m => Int -> Int -> Graph Double -> Int -> m [Double]
avgBanditCost' start goal graph n =
  do iters1 <- banditsPath start goal graph n
     iters2 <- banditsPath start goal graph n
     let costs1' = pathCosts graph $ take 20 iters1
         costs2' = pathCosts graph $ take 20 iters2
         avgs = zipWith (\a b -> (a + b) / 2) costs1' costs2'
     return $ avgs

-- | [ rep1, rep2 .. rep n] where rep n = [Path] (list of iterations)
avgIterCosts' :: Graph Double -> [[Path]] -> [Double]
avgIterCosts' graph reps =
  let w = map (map (cost graph)) reps :: [[Double]]
                    -- cost of each path in each iteration in each repetition
  in avgIterCosts w

avgIterCosts :: [[Double]] -> [Double]
avgIterCosts [] = []
avgIterCosts w =
  let (costs, rest) =
          foldr tr ([], []) w
  in average costs : avgIterCosts rest
  where tr [] a = a
        tr (c:t) (costs', rest') = (c:costs', t:rest')

type Path = [Int]

pathCosts :: Graph Double -> [Path] -> [Double]
pathCosts graph paths = map (cost graph) paths

average :: [Double] -> Double
average costs = theSum / fromIntegral n
  where (theSum, n) = foldl (\(s', n') c -> (s' + c, succ n')) (0, 0) costs

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

