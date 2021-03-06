{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
module Data.UndirectedGraph.Internal where
import Prelude hiding (null)
import Data.UndirectedGraph.RandomUtil
import System.Random ()
import Data.List
import Control.Monad.LazyRandom
import Control.Monad.State
import Control.Arrow (second)
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.IntMap (IntMap, (!))
import Debug.Trace

-- | Graph : nodes indexed by ints, lists of edges
data Graph a = Graph { nodes :: Nodes a
                        , edges :: Edges
                        }
type Edges = IntMap (S.Set Int)
type Nodes a = IntMap a

type Path = [Int]

instance (Show a) => Show (Graph a) where
    show (Graph ns es) =
        "id, value: neighbours\n"
        ++ (unlines . map ppNode $ IM.toList ns)
      where ppNode (i, n) = show i ++ ", " ++ show n ++ ": "
                         ++ unwords (S.toList $ S.map show (es ! i))

printGraph :: (Show a) => Graph a -> IO ()
printGraph = putStrLn . show


type ProbGraph = Graph Rational

-- bounds :: Vector a -> (Int, Int)
-- bounds v = (0, V.length v - 1)
--
null :: Graph g -> Bool
null (Graph ns _) = IM.null ns

size :: Graph a -> (Int, Int)
size (Graph ns es) = (numNodes, numEdges)
  where numNodes = IM.size ns
        numEdges = (sum . map S.size $ IM.elems es) `div` 2


-- | The value and neighbours of the given node, and the graph minus the node.
view :: Show a => Int -> Graph a -> (a, S.Set Int, Graph a)
view i g@(Graph ns es) =
    let val = ns ! i
        neighs = es ! i
        es' = S.foldr (\k m -> IM.adjust (S.delete i) k m) es neighs
        g' = Graph (IM.delete i ns) (IM.delete i es')
    in (val, neighs, g')


neighboursFromList :: [(Int, Int)] -> Int -> Edges
neighboursFromList relations numberOfNodes =
    let a = map (second S.singleton) relations :: [(Int, S.Set Int)]
        b = map (second S.singleton) symmetricRelations :: [(Int, S.Set Int)]
    in IM.fromListWith (S.union) (nub $ a ++ b)
  where symmetricRelations = let (as, bs) = unzip relations in zip bs as

randomEdges :: MonadRandom m => Int -> Int -> m Edges
randomEdges numberOfNodes numberOfConnections =
    do let stronglyConnected = connectStrongly' numberOfNodes
       -- stronglyConnected <- connectStrongly numberOfNodes
       randomNeighbours <-
           (take (numberOfConnections - numberOfNodes)
           . filter (\(a, b) -> a /= succ b && a /= pred b
                                && (not (a == 0 || b == 0)
                                        || (a == 0 && b /= numberOfNodes-1
                                            || b == 0 && a /= numberOfNodes-1)
                                   )
                    )
            ) `liftM` randomPairs  (0, numberOfNodes - 1)
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
cost _            []   = 1
cost (Graph ns _) path = 1 - product (map (\x -> 1 - ns ! x) path)


islands :: Graph a -> [[Int]]
islands g@(Graph ns _) =
    let nis = [0..IM.size ns - 1]
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
    S.toList $ go S.empty i
  where go visited current
          | current `S.member` visited = visited
          | otherwise =
              let myNeighbours = es ! current
              in S.foldl' go (current `S.insert` visited) myNeighbours

simulate initState envTS stateFun =
    evalStateT (go envTS 0) initState
  where go (env:envs) t =
           do result <- stateFun env t
              (result :) `liftM` go envs (t + 1::Int)


