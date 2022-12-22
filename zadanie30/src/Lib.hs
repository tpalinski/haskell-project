module Lib
    ( someFunc
    ) where

data Result = Result {  list :: [Int],
                        partialSum :: Int
                    }

updateList :: a -> [a] -> [a]
updateList x xs = xs ++ [x]

removeLast :: [a] -> [a]
removeLast xs = reverse (tail (reverse xs))

divisors :: Int -> Int
divisors n =  do
    let root = sqrt (fromIntegral n)
    let result = 2 * length [x | x <- [1..floor root], n `mod` x == 0]
    if (n == (floor root)^2 ) then result - 1 else result

mFunction :: Int -> Int -> [Int]
mFunction n k = [divisors x | x <- [n..n+k-1]]

stepMFunction :: Result -> Int -> Int -> Result
stepMFunction (Result { list = prevList, partialSum = prevSum }) n k =
    if n == 1 then
        Result { list = mFunction 2 k, partialSum = prevSum + maximum (mFunction 1 k) + maximum (mFunction 2 k)}
    else
        stepMFunction (Result { list = divisors n : removeLast prevList, partialSum = prevSum + maximum (prevList)}) (n-1) k

sFunction :: Int -> Int -> Int
sFunction u k =
    let mArray = mFunction (u-k+1) k
        recResult = stepMFunction (Result { list = mArray, partialSum = 0 }) (u-k) k
    in partialSum recResult

someFunc :: IO ()
someFunc = do 
    print (sFunction 10000000 100000)
    -- Wynik dla S(1000000 10000) = 175757800
