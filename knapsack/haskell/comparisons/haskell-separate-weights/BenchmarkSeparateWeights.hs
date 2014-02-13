import Criterion.Config
import Criterion.Main

import qualified KnapsackCombined as C
import qualified KnapsackSeparate as S

main = defaultMainWith (defaultConfig{cfgSamples = ljust 10}) (return ()) [
        bcompare [
            bench "knapsack-combined" $ nfIO C.main
          , bench "knapsack-separate" $ nfIO S.main
        ]]
