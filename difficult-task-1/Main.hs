create_list :: Int -> [Int]
create_list n = replicate n 0

numberOfDigits n tab = 
  map (\i -> liczbaCyfr 10 1 i) [1..n]

liczbaCyfr :: (Integral a) => a -> a -> a -> a
liczbaCyfr x y i
  | x `mod` i - y `mod` i == 0 && x > y = round (logBase 10 (fromIntegral x / fromIntegral y))
  | x > y = liczbaCyfr x (10 * y) i
  | otherwise = liczbaCyfr (10 * x) 1 i

findMax :: (Ord a) => [a] -> a
findMax xs = maximum xs

printIndicesOfMax :: (Ord a) => [a] -> IO ()
printIndicesOfMax xs = do
  let maxValue = findMax xs
  mapM_ (\i -> if xs !! i == maxValue then putStrLn ("1/" ++ show (i + 1)) else return ()) [0..length xs - 1]

main :: IO ()
main = do
  let list = create_list 0
  printIndicesOfMax (numberOfDigits 100 list)