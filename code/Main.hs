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
import Control.Monad.LazyRandom
import Control.Monad.Random (fromList, newStdGen)
import Data.List
import Debug.Trace
import Control.Applicative

testGraph :: Graph Double
testGraph = graphFromList [(0,1),(2,3),(4,5),(1,3),(3,5),(1,6),(5,6)]
                          [0,0,0,1,0,0,0]

main :: IO ()
main =
  do graph <- randomGraph 60 100 4
     --let graph = testGraph
     let reps = 100
         iters = 500
     gen <- newStdGen
     starts <- replicateM reps $ fromList $ zip (IM.keys $ nodes graph) [1..]
     goal   <- fromList $ zip (IM.keys $ nodes graph) [1..]
     let res = evalRand (test graph starts goal iters) gen
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

test :: MonadRandom m => Graph Double -> [Int] -> Int -> Int -> m [(String, [Double])]
test graph starts goal iters = do
    banditRes <- mapM (\s -> banditsPath s goal graph iters) starts
    let banditAvgs = avgBanditCost graph banditRes
        dijkstraAvg = avgDijkstraCost starts goal graph
    rndAvg <- replicateM iters $ avgRndCost randomPath starts goal graph
    rndMemAvg <- replicateM iters $ avgRndCost randomPathWithMemory starts goal graph
    return [
             ("dijkstra", replicate iters dijkstraAvg)
           , ("random",   rndAvg)
           , ("random w/mem", rndMemAvg)
           , ("bandits"     , banditAvgs)
           ]

avgDijkstraCost :: [Int] -> Int -> Graph Double -> Double
avgDijkstraCost starts goal graph =
    let res = (flip map) starts $ cost graph . (\start -> dijkstra start goal graph)
    in average res

avgRndCost :: MonadRandom m =>
    (Int -> Int -> Graph Double -> m [Int]) -> [Int] -> Int ->  Graph Double -> m Double
avgRndCost fun starts goal graph =
  do res <- (flip mapM) starts $ liftM (cost graph) . (\start -> fun start goal graph)
     return $ average res



avgBanditCost' :: MonadRandom m => Int -> Int -> Graph Double -> Int -> m [Double]
avgBanditCost' start goal graph n =
  do iters1 <- banditsPath start goal graph n
     iters2 <- banditsPath start goal graph n
     let costs1' = pathCosts graph $ take 20 iters1
         costs2' = pathCosts graph $ take 20 iters2
         avgs = zipWith (\a b -> (a + b) / 2) costs1' costs2'
     return $ avgs

-- | [ rep1, rep2 .. rep n] where rep n = [Path] (list of iterations)
avgBanditCost :: Graph Double -> [[Path]] -> [Double]
avgBanditCost graph reps =
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

