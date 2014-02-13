import Criterion.Config
import Criterion.Main

import qualified KnapsackTake      as T
import qualified KnapsackTakeWhile as W
import qualified KnapsackTakeDrop  as D

main = defaultMainWith (defaultConfig{cfgSamples = ljust 10}) (return ()) [
        bcompare [
            bench "knapsack-take"      $ nfIO T.main
          , bench "knapsack-takeWhile" $ nfIO W.main
          , bench "knapsack-takeDrop"  $ nfIO D.main
        ]]
