
import MagicCP

main = do
  putStrLn "please (ContestId, problemLetter)"
  pId <- read <$> getLine :: IO (Int, Char)
  me <- solveWithAllParsers pId
  case me of
    Just e -> putStrLn $ pprintUC e
    Nothing -> putStrLn "sad"


