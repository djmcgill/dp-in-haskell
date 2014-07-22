import Criterion.Config
import Criterion.Main

import qualified Knapsack      as K
import qualified KnapsackTraverse as T
import qualified KnapsackTakeWhile as W

main = defaultMainWith (defaultConfig{cfgSamples = ljust 10}) (return ()) [
        bcompare [
--             bench "knapsack-traverse" $ nfIO T.main
--           bench "knapsack"      $ nfIO K.main,
            bench "knapsack-takeWhile" $ nfIO W.main
        ]]
