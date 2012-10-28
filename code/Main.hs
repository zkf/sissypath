module Main where
import Graph.Graph
import Graphics.Gloss hiding (Vector)


main :: IO ()
main = undefined
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
renderGraph (width, height) (Nodes ns, Edges es) =
    Pictures [nodesPic, edgesPic]
  where
        widthF = fromIntegral width
        heightF = fromIntegral height
        nsF = V.map (\(v, c) -> (v, toFracs c)) ns
        toFracs (a, b) = (realToFrac a, realToFrac b)
        nodesPic = Color white . Pictures . V.toList $ V.map renderNode nsF
        edgesPic = Color white . Pictures . V.toList $ V.imap renderEdges es
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

