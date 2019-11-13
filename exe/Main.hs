
import MagicCP
import Text.Printf

testsolve wOps wAbs pId = do
  printf "%s %s\n" (show wOps) (show wAbs)
  me <- solveWithAllParsers wOps wAbs pId
  case me of
    Just e -> putStrLn $ pprintUC e
    Nothing -> putStrLn "sad"

main = do
  putStrLn "please (ContestId, problemLetter)"
  pId <- read <$> getLine :: IO (Int, Char)
  testsolve WithOptimizations WithoutAbsents pId
  testsolve WithOptimizations WithAbsents pId
  testsolve WithoutOptimizations WithoutAbsents pId
  testsolve WithoutOptimizations WithAbsents pId


