import Criterion.Config
import Criterion.Main

import qualified KnapsackUnsorted       as U
import qualified KnapsackSortedSlice    as S
import qualified KnapsackSortedGenerate as G
import qualified KnapsackSortedConvert  as C

main = defaultMainWith (defaultConfig{cfgSamples = ljust 10}) (return ()) [
        bcompare [
            bench "knapsack-unsorted"        $ nfIO U.main
          , bench "knapsack-sorted-slice"    $ nfIO S.main
          , bench "knapsack-sorted-generate" $ nfIO G.main
          , bench "knapsack-sorted-convert"  $ nfIO C.main
        ]]
