payment :: Int -> Int
payment week = let days = 5 * week
                   hours = 8 * days
                   in 130 * hours


main :: IO()
main = do
  input <- getLine
  let week = (read input :: Int)
  print(payment week)
