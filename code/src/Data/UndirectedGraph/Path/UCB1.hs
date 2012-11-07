{-# LANGUAGE BangPatterns #-}
module Data.UndirectedGraph.Path.UCB1 where

import Data.UndirectedGraph.Internal
import Data.IntMap hiding (map, update)
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Data.List.Extras (argmax)
import Data.List (find)
import Control.Monad (liftM)
import Control.Monad.Random hiding (fromList)

-- | Given a start and a goal, return an infinite list of (hopefully) improving
-- paths.
banditsPath :: MonadRandom m => Int -> Int -> Graph Double -> Int -> m [[Int]]
banditsPath start goal g@(Graph ns es) n = go bandits n
  where bandits = IM.map (\edgs -> makeUCB1 . S.toList $ edgs) es
        go _ 0 = return []
        go bs i = do (path, bs') <- oneIteration start goal g bs
                     (path :) `liftM` go bs' (i - 1)

oneIteration :: MonadRandom m => Int -> Int -> Graph Double -> Bandits -> m ([Int], Bandits)
oneIteration start goal graph bandits =
  do path <- findPath bandits graph start goal
     return $ (path, updateBandits bandits path goal)

-- | Not nessecarily a valid path ...
findPath :: MonadRandom m => Bandits -> Graph Double -> Int -> Int -> m [Int]
findPath bandits (Graph ns es) start goal = validPath S.empty (traverse bandits start)
  where validPath visited (p:path) =
          do r <- getRandomR (0, 1)
             let done = p == goal || p `S.member` visited || r < (ns ! p)
             if done
                then return [p]
                else (p:) `liftM` validPath (p `S.insert` visited) path

-- | If the path ends at goal then hand out rewards.
updateBandits :: Bandits -> [Int] -> Int -> Bandits
updateBandits bandits path goal =
  let rpath = reverse path
      reward = if head rpath == goal then 1 else 0
  in go rpath reward bandits
  where discount = 0.95
        go (_:[]) _ bandits = bandits
        go (t:i:is) reward bandits =
           let bandits' = adjust (\b -> update b t reward) i bandits
           in go (i:is) (reward * discount) bandits'

move :: UCB1 -> Int
move bandit = select bandit

traverse :: Bandits -> Int -> [Int]
traverse bandits = iterate (move . bandit)
  where bandit = (bandits !)

makeUCB1 :: [Int] -> UCB1
makeUCB1 ns =
  let arm  = Arm (0, 0, 0)
      arms = fromList $ ns `zip` repeat arm
  in UCB1 arms 0 True

data UCB1 = UCB1 (IntMap Arm)
                 Int   -- Total number of plays
                 Bool  -- Are we in the init phase?

newtype Arm = Arm (Double, Double, Int) -- (r, r², number of plays)

type Bandits = IntMap UCB1

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

