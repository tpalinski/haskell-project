create_list :: Int -> [Int]
create_list n = replicate n 0

numberOfDigits n tab = 
  map (\i -> liczbaCyfr 10 1 i) [1..n]

liczbaCyfr :: (Integral a) => a -> a -> a -> a
liczbaCyfr x y i
  | isPower i = 0  -- jeśli i jest potęgą 2 lub potęgą 5 lub iloczynem potęg 2 i 5, zwróć 0
  | x `mod` i - y `mod` i == 0 && x > y = round (logBase 10 (fromIntegral x / fromIntegral y))
  | x > y = liczbaCyfr x (10 * y) i
  | otherwise = liczbaCyfr (10 * x) 1 i

isPower :: (Integral a) => a -> Bool
isPower x = x > 1 && (x == 2 || x == 5 || (x `mod` 2 == 0 && isPower (x `div` 2)) || (x `mod` 5 == 0 && isPower (x `div` 5)))

findMax :: (Ord a) => [a] -> a
findMax xs = maximum xs

printIndicesOfMax :: (Ord a) => [a] -> IO ()
printIndicesOfMax xs = do
  let maxValue = findMax xs
  mapM_ (\i -> if xs !! i == maxValue then putStrLn ("1/" ++ show (i + 1)) else return ()) [0..length xs - 1]

main :: IO ()
main = do
  let list = create_list 0
  printIndicesOfMax (numberOfDigits 16 list)