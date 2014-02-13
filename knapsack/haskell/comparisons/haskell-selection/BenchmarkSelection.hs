import Criterion.Config
import Criterion.Main

import qualified KnapsackList as L
import qualified KnapsackMap as M

main = defaultMainWith (defaultConfig{cfgSamples = ljust 10}) (return ()) [
        bcompare [
            bench "knapsack-list" L.main
          , bench "knapsack-map"  M.main
        ]]
