
import qualified NaiveParse             as N (readProblem)
import qualified AttoparsecDecimalParse as AD (readProblem)
import qualified AttoparsecUIntParse    as AU (readProblem)

import Criterion.Main

fileName = "test_problem_1.data"

main = defaultMain [
   bcompare [
     bench "Attoparsec.decimal" $ AD.readProblem fileName
   , bench "Attoparsec.uint"    $ AU.readProblem fileName
   , bench "Prelude.read"       $ N.readProblem  fileName
   ]
 ]