module Lib
    ( someFunc
    ) where
    
zbiorPotegowy :: [Int] -> [[Int]]
zbiorPotegowy [] = [[]] --jezeli podajemy na wejscie zbior pusty to zwraca zbior jedynie ze zbiorem pustym
zbiorPotegowy (n:x) = map (n:) (zbiorPotegowy x) ++ zbiorPotegowy x
-- n - jakis zbior ktory podajemy na wejscie;
-- bierzemy po kolei wszystkie elementy zbioru i dostajemy wszystkie zbiory zawierajce ten element + zbior pusty z ostatnim elementem
-- na koncu otrzymujemy zbior zawierajacy wszystkie zbiory ktore otrzymalismy

someFunc :: IO ()
someFunc = print(zbiorPotegowy [4, 7, 10, 3])

