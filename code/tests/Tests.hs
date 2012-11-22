module Main where
import Data.UndirectedGraph
import Data.UndirectedGraph.Internal
import Data.UndirectedGraph.Path.Bruteforce
import Data.UndirectedGraph.Path.Dijkstra
import Data.UndirectedGraph.Path.UCB1
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (path)
import Data.List (sort, find)
import Data.IntMap (empty)
import Control.Monad.Random

main :: IO ()
main = defaultMain
  [ testGroup "Enumerate paths"
    [ testCase "empty"
        $ enumeratePaths 0 1 (Graph empty empty) @?= []
    , testCase "minimal"
        $ enumeratePaths 0 1 (graphFromList [(0,1)] ones) @?= [[0, 1]]
    , testCase "appendix"
        $ enumeratePaths 0 1 (graphFromList [(0,1),(0,2)] ones)
        @?= [[0, 1]]
    , testCase "triangle"
        $ sort (enumeratePaths 0 2 (graphFromList [(0,1),(0,2),(1,2)] ones))
          @?= [[0,1,2],[0,2]]
    , testCase "diamond"
        $ sort (enumeratePaths 0 2
                $ graphFromList [(0,1),(1,2),(0,2),(0,3),(3,2)] ones)
          @?= [[0,1,2],[0,2],[0,3,2]]
    , testCase "triangle appendix"
        $ sort (enumeratePaths 0 1
               $ graphFromList [(0,1),(0,2),(2,3),(3,4),(4,2)] ones)
          @?= [[0,1]]
    , testCase "lattice 3x3"
        $ sort (enumeratePaths 0 8
                $ graphFromList [(0,1),(0,3),(1,4),(1,2),(2,5),(3,6)
                                ,(3,4),(4,5),(4,7),(5,8),(6,7),(7,8)
                                ]
                                ones)
          @?= [ [0,1,2,5,4,3,6,7,8], [0,1,2,5,4,7,8], [0,1,2,5,8]
              , [0,1,4,3,6,7,8], [0,1,4,5,8], [0,1,4,7,8]
              , [0,3,4,1,2,5,8], [0,3,4,5,8], [0,3,4,7,8]
              , [0,3,6,7,4,1,2,5,8], [0,3,6,7,4,5,8], [0,3,6,7,8]]
    ]
  , testGroup "Calculate cost of path"
    [ testCase "singleton" $ cost (graphFromList [(0,1)] ones) [0,1] @?= 1
    , testCase "square"
        $ cost (graphFromList [(0,1),(0,2),(1,3),(2,3)] [0,1,0,0]) [0,1]
            @?= 1.0
    , testCase "w1"
        $ cost tricky [0,4,5,3] @?= 0.75
    , testCase "w2"
        $ cost tricky [0,1,2,3] @?= 1
    ]
  , testGroup "Brute force shortest path" $ testPaths bruteForceOptimalPath
  , testGroup "Dijkstra" $ testPaths dijkstra
  , testGroup "Bandits"
    [ testCase "square 1" $ do gen <- newStdGen
                               let res = evalRand (banditsPath 0 3 square) gen
                               res !! 500 @?= [0,2,3]
    , testCase "square 2" $ do gen <- newStdGen
                               let res = evalRand (banditsPath 0 1 square) gen
                               res !! 500 @?= [0,1]
    , testCase "tricky" $ do gen <- newStdGen
                             let Just res = find (not . Prelude.null)
                                       $ drop 500
                                       $ evalRand (banditsPath 0 3 tricky) gen
                             res @?= [0,4,5,3]
    ]
  ]
  where testPaths pathFun =
            [ testCase "square 1" $ pathFun 0 3 square @?= [0,2,3]
            , testCase "square 2" $ pathFun 0 1 square @?= [0,1]
            , testCase "tricky"   $ pathFun 0 3 tricky @?= [0,4,5,3]
            ]

ones :: [Double]
ones = [1,1..]

square, tricky :: Graph Double
square = graphFromList [(0,1),(0,2),(1,3),(2,3)] [0,1,0,0]
tricky = graphFromList [(0,1),(1,2),(2,3),(0,4),(4,5),(5,3)]
                       [0,1,0,0,0.5,0.5]
