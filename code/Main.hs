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
import Data.Monoid
import System.Environment

testGraph :: HazardGraph
testGraph = graphFromList [(0,1),(2,3),(4,5),(1,3),(3,5),(1,6),(5,6)]
                          [0,0,0,0,0,0,0]

main :: IO ()
main =
  do -- graph <- randomEmptyGraph 7 14
     [repsS, itersS] <- getArgs
     let reps = read repsS
         iters = read itersS
         halfIters = round $ fromIntegral iters / 2
     let graphs1 = (insertHazards testGraph 0.3) >>= return . replicate halfIters
     let graphs2 = (insertHazards testGraph 0.3) >>= return . replicate halfIters
     let graphTS = liftM concat $ sequence [graphs1, graphs2]
     graphsTS <- replicateM reps graphTS
     gen <- newStdGen
     starts <- replicateM reps
               $ fromList
               $ zip (IM.keys $ nodes testGraph) [1, 1..]
     --graphs <- replicateM reps graph
     --goal   <- fromList $ zip (IM.keys $ nodes $ head graphs) [1, 1..]
     let goal = 6
     let res = evalRand (test graphsTS starts goal iters) gen
         ppRes = ("iteration" : map show [1 :: Int .. iters]) : map pp res
     putStrLn (unlines . map unwords $ transpose ppRes)

pp :: Show a => (String, [a]) -> [String]
pp (label, d) = label : map show d

graphUpdate :: MonadRandom m => Int -> Graph Double -> Int -> m (Graph Double)
graphUpdate when graph t = do
    if when == t
       then insertHazards (removeHazards graph) 0.4
       else return graph

type HazardGraph = Graph Double
type TimeSeries a = [a]

test :: MonadRandom m
    => [TimeSeries HazardGraph] -> [Int] -> Int -> Int -> m [(String, [Double])]
test graphsTS starts goal iters = do
    -- let upd = graphUpdate (round $ fromIntegral iters / 2)
    let upd graph _ = return graph
    let getCosts graphTS paths = zipWith cost graphTS paths
    banditPaths <- zipWithM
                (\graphTS start ->
                    banditsPath' start goal graphTS)
                graphsTS starts
    let banditCosts = zipWith getCosts graphsTS banditPaths
    let banditAvgs = take iters $ map average $ transpose banditCosts

        dijkstraPaths = zipWith
                          (\graphTS start -> iterDijkstra start goal graphTS)
                          graphsTS
                          starts
        dijkstraCost = zipWith getCosts graphsTS dijkstraPaths
        dijkstraAvgs = map average . transpose $ dijkstraCost
    --rndAvg <- replicateM iters $  randomPath starts goal graphs
    rndRes <- zipWithM (\graphTS start -> iterRandomPath start goal graphTS)
                       graphsTS
                       starts
    let rndCosts = zipWith getCosts graphsTS rndRes
        rndAvgs = map average . transpose $ rndCosts
    --rndMemAvg <- replicateM iters
    --             $ avgRndCost randomPathWithMemory starts goal graphs
    return [
             ("dijkstra", dijkstraAvgs)
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

