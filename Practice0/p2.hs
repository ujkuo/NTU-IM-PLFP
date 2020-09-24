area :: Float -> Float
area r = (22/7) * r * r

main :: IO()
main = do
  input <- getLine
  let r = (read input :: Float)
  print(area r)
