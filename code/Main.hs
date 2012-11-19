module Main where
import Data.UndirectedGraph
import Data.UndirectedGraph.Path.Random
import Data.UndirectedGraph.Path.Bruteforce
import Data.UndirectedGraph.Path.Dijkstra
import Data.UndirectedGraph.Path.UCB1
-- import Graphics.Gloss hiding (Vector)
import qualified Data.IntMap as IM
import Control.Monad
import Control.Monad.LazyRandom
import Control.Monad.Random (fromList, newStdGen)
import Data.List

testGraph :: Graph Double
testGraph = graphFromList [(0,1),(2,3),(4,5),(1,3),(3,5),(1,6),(5,6)]
                          [0,0,0,1,0,0,0]

main :: IO ()
main =
  do -- graph <- randomGraph 60 100 4
     let reps = 5
         iters = 10
     graphs <- replicateM reps
               $ graphFromList [(0,1),(2,3),(4,5),(1,3),(3,5),(1,6),(5,6)]
               `liftM` replicateM 7 ( fromList $ zip [0,1] [1, 1..])
     print graphs
     gen <- newStdGen
     starts <- replicateM reps
               $ fromList
               $ zip (IM.keys $ nodes $ head graphs) [1, 1..]
     --graphs <- replicateM reps graph
     goal   <- fromList $ zip (IM.keys $ nodes $ head graphs) [1, 1..]
     let res = evalRand (test graphs starts goal iters) gen
         ppRes = ("iteration" : map show [1 :: Int .. iters]) : map pp res
     putStrLn (unlines . map unwords $ transpose ppRes)

pp :: Show a => (String, [a]) -> [String]
pp (label, d) = label : map show d

test :: MonadRandom m
    => [Graph Double] -> [Int] -> Int -> Int -> m [(String, [Double])]
test graphs starts goal iters = do
    let getCosts paths = zipWith cost graphs paths
    banditCosts <- zipWithM
                (\graph start -> liftM getCosts $ banditsPath start goal graph)
                graphs starts
    let banditAvgs = map average $ transpose banditCosts
        dijkstraRes = zipWith (\graph start -> dijkstra start goal graph)
                              graphs starts
        dijkstraCost = average $ zipWith cost graphs dijkstraRes
    --rndAvg <- replicateM iters $  randomPath starts goal graphs
    rndRes <- replicateM iters
              $ zipWithM (\graph start -> randomPath start goal graph)
                         graphs starts
    let rndCosts = map (zipWith cost graphs) rndRes
        rndAvgs = map average rndCosts
    --rndMemAvg <- replicateM iters
    --             $ avgRndCost randomPathWithMemory starts goal graphs
    return [
             ("dijkstra", replicate iters dijkstraCost)
           , ("random",   rndAvgs)
--           , ("random w/mem", rndMemAvg)
           , ("bandits"     , banditAvgs)
           ]

avgDijkstraCost :: [Int] -> Int -> Graph Double -> Double
avgDijkstraCost starts goal graph =
    let res = (flip map) starts
              $ cost graph . (\start -> dijkstra start goal graph)
    in average res

avgRndCost :: MonadRandom m
    => (Int -> Int -> Graph Double -> m [Int])
    -> [Int] -> Int ->  Graph Double -> m Double
avgRndCost fun starts goal graph =
  do res <- (flip mapM) starts
            $ liftM (cost graph) . (\start -> fun start goal graph)
     return $ average res

-- | [ rep1, rep2 .. rep n] where rep n = [Path] (list of iterations)
avgBanditCost :: [[Double]] -> [Double]
avgBanditCost = map average . transpose

average :: [Double] -> Double
average costs = theSum / fromIntegral n
  where (theSum, n) = foldl (\(s', n') c -> (s' + c, succ n'))
                            (0, 0::Int)
                            costs

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

