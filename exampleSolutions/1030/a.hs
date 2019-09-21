main = do
  _ <- getLine
  l <- words <$> getLine
  if all (== "0") l then putStrLn "EASY" else putStrLn "HARD"


-- f inp = let l = words ((lines inp)!!1) in if all (== "0") l then "EASY\n" else "HARD\n"
