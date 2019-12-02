module Main

import Data.String

maybePut: Either FileError String -> String
maybePut (Left l) = "Nop"
maybePut (Right r) = r

f: String -> Maybe Int
f x = parseInteger x

filterN: List (Maybe Int) -> List Int
filterN [] = []
filterN (Nothing :: xs) = filterN xs
filterN ((Just x) :: xs) = x :: filterN xs

fuel: Int -> Int
fuel x = (div x 3 ) -2

fuel2: Int -> Int
fuel2 cargo = if cargo <=6 then 0 else  fuel2 (fuel cargo)+ (fuel cargo)

main : IO ()
main = do
  s <- readFile "./data.txt"
  print $ sum $ map fuel $ filterN $ (map f) $ (lines $ maybePut s)
  putStrLn ""
  print $ sum $ map fuel2 $ filterN $ (map f) $ (lines $ maybePut s)
