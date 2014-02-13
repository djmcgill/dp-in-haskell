import Criterion.Config
import Criterion.Main

import qualified KnapsackUnfoldR    as U
import qualified KnapsackTraverse   as T
import qualified KnapsackGenerateM  as G

main = defaultMainWith (defaultConfig{cfgSamples = ljust 20}) (return ()) [
        bcompare [
            bench "knapsack-unfoldr"    $ nfIO U.main
          , bench "knapsack-traverse"   $ nfIO T.main
          , bench "knapsack-generateM"  $ nfIO G.main
        ]]
