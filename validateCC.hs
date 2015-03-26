everyOther :: [Int] -> [Int]
everyOther xs = map snd (filter (odd . fst) indexed)
    where indexed = indexedInts xs

indexedInts :: [Int] -> [(Int, Int)]
indexedInts xs = zip [0..] xs

strToListOfInts :: String -> [Int]
strToListOfInts xs = [read (x:"") * 1 | x <- xs]

sumDigits :: Int -> Int
sumDigits x
    | x < 10 = x
    | otherwise = mod x 10 + sumDigits (div x 10)

checkSum :: String -> Int
checkSum cc = sum (map sumDigits (map (*2) (everyOther $ strToListOfInts cc)))

validateCC :: String -> Bool
validateCC cc = (mod (checkSum cc) 10) == 0