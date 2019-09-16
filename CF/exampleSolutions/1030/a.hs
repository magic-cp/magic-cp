main = do
  _ <- getLine
  l <- words <$> getLine
  if all (== "0") l
     then putStrLn "EASY"
     else putStrLn "HARD"
