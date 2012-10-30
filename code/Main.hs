module Main where
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.UndirectedGraph
import Data.UndirectedGraph.Path.Random
import Data.UndirectedGraph.Path.Bruteforce
import Graphics.Gloss hiding (Vector)


main :: IO ()
main =
  do g <- randomGraph 30 60
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


displayGraph :: Graph (Char, (Double, Double)) -> IO ()
displayGraph g = display window black (renderGraph dims g)
  where window = InWindow "graph" dims (0, 0)
        dims   = (800, 600)

renderGraph :: (Int, Int) -> Graph (Char, (Double, Double)) -> Picture
renderGraph (width, height) graph =
    Pictures [nodesPic, edgesPic]
  where
        widthF = fromIntegral width
        heightF = fromIntegral height
        ns = nodes graph
        es = edges graph
        nsF = V.map (\(v, c) -> (v, toFracs c)) ns
        toFracs (a, b) = (realToFrac a, realToFrac b)
        nodesPic = Color white . Pictures . V.toList $ V.map renderNode nsF
        edgesPic = Color white . Pictures . V.toList
                   $ V.imap renderEdges es
        renderNode (_,(x,y)) = Translate ((x - 0.5) * widthF)
                                         ((y - 0.5) * heightF)
                               $ circleSolid 5
        renderEdges i fs = Pictures $ map (renderEdge i) fs
        renderEdge i e   =
            let (_,(x0, y0)) = nsF ! i
                (_,(x1, y1)) = nsF ! e
                x0' = (x0 - 0.5) * widthF
                y0' = (y0 - 0.5) * heightF
                x1' = (x1 - 0.5) * widthF
                y1' = (y1 - 0.5) * heightF
            in Line [(x0', y0'), (x1', y1')]

