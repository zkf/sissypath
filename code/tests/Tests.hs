module Main (main) where
import Data.UndirectedGraph
import Data.UndirectedGraph.Internal
import Data.UndirectedGraph.Path.Bruteforce
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (path)
import Data.List (sort)
import qualified Data.Vector as V

main :: IO ()
main = defaultMain
  [ testGroup "Enumerate paths"
    [ testCase "empty"
        $ enumeratePaths 0 1 (Graph V.empty V.empty) @?= []
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
        $ cost (graphFromList [(0,1),(1,2),(2,3),(0,4),(4,5),(5,3)]
                              [0,1,0,0,0.5,0.5])
               [0,4,5,3] @?= 0.75
    , testCase "w2"
        $ cost (graphFromList [(0,1),(1,2),(2,3),(0,4),(4,5),(5,3)]
                              [0,1,0,0,0.5,0.5])
               [0,1,2,3] @?= 1
    ]
  , testGroup "Brute force shortest path"
    [ testCase "square 1" $ bruteForceOptimalPath 0 3
        (graphFromList [(0,1),(0,2),(1,3),(2,3)] [0,1,0,0])
        @?= [0,2,3]
    , testCase "square 2" $ bruteForceOptimalPath 0 1
        (graphFromList [(0,1),(0,2),(1,3),(2,3)] [0,1,0,0])
        @?= [0,1]
    ]
  ]
  where ones = [1::Double,1..]
