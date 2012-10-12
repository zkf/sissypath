{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
module Graph
    ( Nodes(..)
    , Edges(..)
    , Colour(..)
    ) where
import RandomUtil
import Control.Arrow (first)
import System.Random ()
import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V (write)
import Data.Vector (Vector, accum, indexed, slice, ifoldl', (!), (//))
import Control.Monad.Random
import Control.Monad.State
import Graphics.Gloss

-- | Graph : nodes indexed by ints, lists of edges
type Graph a = (Nodes a, Edges)
data Edges = Edges !(Vector [Int]) deriving (Show)
data Nodes a = Nodes !(Vector a) deriving (Show, Eq)

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

randomGraph :: MonadRandom m => m (Graph Char)
randomGraph =
  do let nodesList = take 20 ['a'..]
         nodes = Nodes $ V.fromList nodesList
     edges <- randomEdges nodes 22
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

renderGraph :: Graph (Char, (Double, Double)) -> Picture


data Colour = Red | Green | Blue  deriving (Eq, Ord, Enum, Show, Bounded)

instance Random Colour where
    randomR (lo, hi) gen = first toEnum (randomR (fromEnum lo, fromEnum hi) gen)
    random = randomR (minBound, maxBound)

getRandomColours :: MonadRandom m => Int -> m (Nodes Colour)
getRandomColours n =
    do vs <- getRandoms
       return . Nodes . V.fromList $ take n vs










