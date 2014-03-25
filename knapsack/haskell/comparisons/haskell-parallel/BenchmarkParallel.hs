import Criterion.Config
import Criterion.Main

import qualified KnapsackParallel   as P
import qualified KnapsackSequential as S

main = defaultMainWith (defaultConfig{cfgSamples = ljust 50}) (return ()) [
        bcompare [
            bench "knapsack-parallel"   P.main
          , bench "knapsack-sequential" S.main
        ]]
