{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
module Data.UndirectedGraph.Internal where
import Data.UndirectedGraph.RandomUtil
import System.Random ()
import Data.List
import qualified Data.Vector as V
import Data.Vector (Vector, accum, (!))
import Control.Monad.Random
import Control.Monad.State

-- | Graph : nodes indexed by ints, lists of edges
data Graph a = Graph { nodes :: Nodes a
                        , edges :: Edges
                        }
type Edges = Vector [Int]
type Nodes a = Vector a

instance (Show a) => Show (Graph a) where
    show (Graph ns es) =
        "id, value: neighbours\n"
        ++ (unlines . V.toList $ V.imap ppNode ns)
      where ppNode i n = show i ++ ", " ++ show n ++ ": "
                         ++ unwords (map show (es ! i))

printGraph :: (Show a) => Graph a -> IO ()
printGraph = putStrLn . show


type ProbGraph = Graph Rational

bounds :: Vector a -> (Int, Int)
bounds v = (0, V.length v - 1)


neighboursFromList :: [(Int, Int)] -> Int -> Edges
neighboursFromList relations numberOfNodes =
    let a = V.replicate numberOfNodes []
    in accum (flip (:)) a (nub $ relations ++ symmetricRelations)
  where symmetricRelations = let (as, bs) = unzip relations in zip bs as

randomEdges :: MonadRandom m => Int -> Int -> m Edges
randomEdges numberOfNodes numberOfConnections =
    do let stronglyConnected = connectStrongly' numberOfNodes
       -- stronglyConnected <- connectStrongly numberOfNodes
       randomNeighbours <- randomPairs (numberOfConnections - numberOfNodes) (0, numberOfNodes - 1)
       let theEdges = neighboursFromList (stronglyConnected ++ randomNeighbours) numberOfNodes
       return theEdges

-- | Given the number of nodes, return a list of relations such that, if the
-- connections are taken to be bidirectional, the resulting graph is strongly
-- connected.
connectStrongly' :: Int -> [(Int, Int)]
connectStrongly' num =
    foldl' (\acc x -> (x, x+1):acc) [(0, num - 1)] [0 .. num - 2]


connectStrongly :: MonadRandom m => Int -> m [(Int, Int)]
connectStrongly num =
    go [0..top] []
  where go [] conns = return conns
        go (n:ns) conns =
            do r <- (head . dropWhile (\x -> x == n || x `elem` getRelated n conns)) `liftM` getRandomRs (0, top)
               go ns ((n,r):conns)
        top = num - 1
        getRelated n conns = foldl
            (\acc (a, b) -> if a == n
                               then b:acc
                               else if b == n
                                       then a:acc
                                       else acc)
            []
            conns



cost :: Graph Double -> [Int] -> Double
cost (Graph ns _) path = 1 - product (map (\x -> 1 - ns ! x) path)


islands :: Graph a -> [[Int]]
islands g@(Graph ns _) =
    let nis = [0..V.length ns - 1]
    in go nis
  where go [] = []
        go unvisited@(n:_) =
           let reachable = traverseFrom n g
               unreachable = unvisited \\ reachable
           in reachable : go unreachable

-- | Perform a complete traversal of the graph from the given node index.
-- Returns a list of all nodes reachable from the given node.
traverseFrom :: Int -> Graph a -> [Int]
traverseFrom i (Graph _ es) =
    go [] i
  where go visited current
          | current `elem` visited = visited
          | otherwise =
              let myNeighbours = es ! current
              in foldl' go (current:visited) myNeighbours



