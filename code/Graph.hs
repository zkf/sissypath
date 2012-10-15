{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
module Graph where
import RandomUtil
import Control.Arrow (first)
import System.Random ()
import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V (write)
import Data.Vector (Vector, accum, indexed, slice, ifoldl', (!), (//))
import Control.Monad.Random
import Control.Monad.State
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

-- | Graph : nodes indexed by ints, lists of edges
type Graph a = (Nodes a, Edges)
newtype Edges = Edges (Vector [Int]) deriving (Show, Eq)
newtype Nodes a = Nodes (Vector a) deriving (Show, Eq)

printGraph :: (Show a) => Graph a -> IO ()
printGraph (Nodes ns, Edges es) =
  do putStrLn "id, value: neighbours"
     V.mapM_ putStrLn $ V.imap ppNode ns
  where ppNode i n = show i ++ ", " ++ show n ++ ": "
                            ++  unwords (map show (es ! i))


type ProbGraph = Graph Rational

bounds :: Vector a -> (Int, Int)
bounds v = (0, V.length v - 1)


neighboursFromList :: [(Int, Int)] -> Int -> Edges
neighboursFromList relations numberOfNodes =
    let a = V.replicate numberOfNodes []
    in Edges $ accum (flip (:)) a (nub $ relations ++ symmetricRelations)
  where symmetricRelations = let (as, bs) = unzip relations in zip bs as

randomEdges :: MonadRandom m => Nodes a -> Int -> m Edges
randomEdges (Nodes nodes) numberOfConnections =
    do neighbourList <- randomPairs numberOfConnections (0, numberOfNodes - 1)
       let edges = neighboursFromList neighbourList numberOfNodes
       return edges
  where numberOfNodes = V.length nodes

randomGraph :: MonadRandom m => Int -> m (Graph Int)
randomGraph size =
  do let nodesList = take size [0..]
         nodes = Nodes $ V.fromList nodesList
     edges <- randomEdges nodes (round $ fromIntegral size * 1.3)
     return (nodes, edges)

randomGraph' :: MonadRandom m => Int -> m (Graph (Char, (Double, Double)))
randomGraph' size =
  do thePoints <- take size `liftM` points
     let nodes = Nodes . V.fromList $ take size (zip ['a'..] thePoints)
         neighbours =  map (findNeighbours $ zip [0..] thePoints) thePoints
         relations = concat $ zipWith (\ns i -> map (\n -> (i, n)) ns) neighbours [0..]
         edges = neighboursFromList relations size
     return (nodes, edges)
  where dist (x1, y1) (x2, y2) = sqrt $ (x2 - x1)**2 + (y2 - y1)**2
        findNeighbours :: [(Int, (Double, Double))] -> (Double, Double) -> [Int]
        findNeighbours ps p = snd . unzip . take 2 . sort
                              . map (\(i, q) -> (dist p q, i))
                              $ filter (\(_, q) -> q /= p) ps
        points =
          do as <- getRandomRs (0, 1::Double)
             bs <- getRandomRs (0, 1::Double)
             return $ zip as bs

-- renderGraph :: Graph (Char, (Double, Double)) -> Picture


data Colour = Red | Green | Blue  deriving (Eq, Ord, Enum, Show, Bounded)

instance Random Colour where
    randomR (lo, hi) gen = first toEnum (randomR (fromEnum lo, fromEnum hi) gen)
    random = randomR (minBound, maxBound)

getRandomColours :: MonadRandom m => Int -> m (Nodes Colour)
getRandomColours n =
    do vs <- getRandoms
       return . Nodes . V.fromList $ take n vs



paths :: Int -> Int -> Graph a -> [[Int]]
paths _ _ (Nodes ns, _)
  | V.null ns = []
paths start goal (Nodes ns, Edges es) =
    go [] start
  where go visited current
          | current == goal = [[goal]]
          | current `elem` visited = []
          | otherwise =
              let myNeighbours = if null visited
                                     then es ! current
                                     else nextNeighbours current (head visited)
              in if null myNeighbours then []
                     else let furtherPaths = concatMap (go (current:visited)) myNeighbours
                          in map (current :) $ filter (not . null) furtherPaths

        nextNeighbours me prev = filter (/= prev) $ es ! me

islands :: Graph a -> [Int]
islands g@(Nodes ns, Edges es) =
    let reachable = traverseFrom 0 g
    in filter (`notElem` reachable) [0..V.length ns - 1]

traverseFrom :: Int -> Graph a -> [Int]
traverseFrom i (_, Edges es) =
    go [] i
  where go visited current
          | current `elem` visited = visited
          | otherwise =
              let myNeighbours = es ! current
              in foldl' go (current:visited) myNeighbours

graphFromList :: [(Int, Int)] -> (Int -> a) -> Graph a
graphFromList rels f =
    let nodes = map f . nub . uncurry (++) $ unzip rels
        len = length nodes
        edges = neighboursFromList rels len
    in (Nodes $ V.fromList nodes, edges)


main :: IO ()
main = --tests
  do g <- randomGraph 60
     print . last $ paths 0 1 g

tests :: IO ()
tests = defaultMain
  [ testGroup "Trivial graphs"
    [ testCase "empty" $ paths 0 1 (Nodes V.empty, Edges V.empty) @?= []
    , testCase "minimal" $ paths 0 1 (graphFromList [(0,1)] id) @?= [[0, 1]]
    , testCase "appendix" $ paths 0 1 (graphFromList [(0,1),(0,2)] id) @?= [[0, 1]]
    , testCase "triangle" $ sort (paths 0 2 (graphFromList [(0,1),(0,2),(1,2)] id)) @?= [[0,1,2],[0,2]]
    , testCase "diamond"
        $ sort (paths 0 2 $ graphFromList [(0,1),(1,2),(0,2),(0,3),(3,2)] id)
          @?= [[0,1,2],[0,2],[0,3,2]]
    , testCase "triangle appendix"
        $ sort (paths 0 1 $ graphFromList [(0,1),(0,2),(2,3),(3,4),(4,2)] id)
          @?= [[0,1]]
    , testCase "lattice 3x3"
        $ sort (paths 0 8 $ graphFromList [(0,1),(0,3),(1,4),(1,2),(2,5),(3,6)
                                          ,(3,4),(4,5),(4,7),(5,8),(6,7),(7,8)]
                                          id)
          @?= [ [0,1,2,5,4,3,6,7,8], [0,1,2,5,4,7,8], [0,1,2,5,8]
              , [0,1,4,3,6,7,8], [0,1,4,5,8], [0,1,4,7,8]
              , [0,3,4,1,2,5,8], [0,3,4,5,8], [0,3,4,7,8]
              , [0,3,6,7,4,1,2,5,8], [0,3,6,7,4,5,8], [0,3,6,7,8]]
    ]
  ]


