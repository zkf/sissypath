{-# LANGUAGE BangPatterns #-}
module Data.UndirectedGraph.Path.UCB1 where

import Data.UndirectedGraph.Internal
import Data.IntMap hiding (map, update)
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Data.List.Extras (argmax)
import Data.List (find)
import Control.Monad (liftM)
import Control.Monad.State
import Control.Monad.LazyRandom hiding (fromList)

data UCB1 = UCB1 (IntMap Arm)
                 Int   -- Total number of plays
                 Bool  -- Are we in the init phase?

newtype Arm = Arm (Double, Double, Int) -- (r, r², number of plays)

type Bandits = IntMap UCB1

type BState g = StateT Bandits g Path

-- | Given a start and a goal, return an infinite list of (hopefully) improving
-- paths.
banditsPath :: MonadRandom m => Int -> Int -> Graph Double -> m [[Int]]
banditsPath start goal g@(Graph ns es) = liftM (map reverse) $ evalStateT go bandits
  where bandits = IM.map (\edgs -> makeUCB1 . S.toList $ edgs) es
        go = do rpath <- oneIteration start goal g
                liftM (rpath :) go

oneIteration :: MonadRandom g => Int -> Int -> Graph Double -> BState g
oneIteration start goal graph =
  do rpath <- findPath graph start goal
     modify (updateBandits rpath goal)
     -- return (if head rpath /= goal then [] else rpath)
     return rpath

-- | Not nessecarily a valid path ...
findPath :: MonadRandom m => Graph Double -> Int -> Int -> BState m
findPath (Graph ns es) start goal =
    do bandits <- get
       validPath S.empty (traverse bandits start)
  where validPath visited (n:path)
          | n == goal = return [n]
          | n `S.member` visited = return []
          | otherwise =
              do r <- getRandomR (0, 1)
                 if r < (ns ! n)
                    then return [n]
                    else validPath (n `S.insert` visited) path >>= return.(++ [n])

-- | If the path ends at goal then hand out rewards.
updateBandits :: [Int] -> Int -> Bandits -> Bandits
updateBandits rpath goal bandits =
  let reward = if head rpath == goal then 1 else -1
  in go rpath reward bandits
  where discount = 0.95
        go (_:[]) _ bandits = bandits
        go (t:i:is) reward bandits =
           let bandits' = adjust (\b -> update b t reward) i bandits
           in go (i:is) (reward * discount) bandits'

traverse :: Bandits -> Int -> [Int]
traverse bandits = iterate (select . bandit)
  where bandit = (bandits !)

makeUCB1 :: [Int] -> UCB1
makeUCB1 ns =
  let arm  = Arm (0, 0, 0)
      arms = fromList $ ns `zip` repeat arm
  in UCB1 arms 0 True


select :: UCB1 -> Int
select (UCB1 arms plays initPhase) =
   if not initPhase
        then let fun j = armValue (arms ! j) plays
             in argmax fun $ keys arms
        else case find (\(_, Arm (_, _, n)) -> n == 0) $ assocs arms of
                  Nothing -> select (UCB1 arms plays False)
                  Just (i, _) -> i

armValue :: Arm -> Int -> Double
armValue (Arm (r, r2, count)) totalCount = avg + sqrt (logTerm * min (1/4) v)
  where v     = var + sqrt (2 * logTerm)
        avg   = r * divByCount'
        logTerm   = log (fromIntegral totalCount) * divByCount'
        count' = fromIntegral count
        var = r2 * divByCount' - avg^(2::Int)
        divByCount' = 1 / count'

update :: UCB1 -> Int -> Double -> UCB1
update (UCB1 arms plays initPhase) index reward =
    let arms' = adjust (updateArm reward) index arms
    in UCB1 arms' (plays + 1) initPhase

updateArm :: Double -> Arm -> Arm
updateArm reward (Arm (r, r2, count)) = Arm $ (r + reward, r2 + reward^(2::Int), count + 1)



takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ [] = return []
takeWhileM p (x:xs) =
  do b <- p x
     if b then (x:) `liftM` takeWhileM p xs
          else return []

