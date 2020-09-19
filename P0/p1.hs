myeven :: Int -> Bool
myeven x = x `mod` 2 == 0

main :: IO()
main = do
  input <- getLine
  let x = (read input :: Int)
  print(myeven x)
