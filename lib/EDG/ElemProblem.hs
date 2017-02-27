
module EDG.ElemProblem where

import EDG.Elements

-- | Is the main function in the app
runTestProblem :: IO ()
runTestProblem = do
  solveProblem testProblem
  return ()
