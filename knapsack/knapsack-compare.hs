import Criterion.Config
import Criterion.Main
import System.Process
import Text.Printf

mkProblem :: Int -> String
mkProblem = printf "C:\\code\\phd\\dp-in-haskell\\knapsack\\%dk_instance.data"

data Lang = C99 | Haskell | Java7 deriving Show

cMain problem = callProcess "C:\\code\\phd\\dp-in-haskell\\knapsack\\c\\Knapsack.exe"        [problem]
hMain problem = callProcess "C:\\code\\phd\\dp-in-haskell\\knapsack\\haskell\\Knapsack.exe"  [problem]
jMain problem = callProcess "java" ["Knapsack", problem]

main = defaultMainWith (defaultConfig{cfgSamples = ljust 10}) (return ()) [bcompare $ [
	benches C99,
	benches Haskell,
	benches Java7]]

getMain C99 = cMain
getMain Haskell = hMain
getMain Java7 = jMain

mkName :: Lang -> String
mkName = printf "knapsack-%s" . show

sizes = [5,10..50]
benches :: Lang -> Benchmark
benches lang = bgroup ("knapsack-" ++ show lang) $ map (\n -> bench (mkName lang) (getMain lang (mkProblem n))) sizes