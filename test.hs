
test :: ( Num a ) => [a] -> String
test [] = "None"
test x:[] = "One"
test x:y:[] = "Two"
test x:_ = "MANY!"

cylinder :: ( RealFloat a ) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

myReplicate :: (Num i, Ord i) => i -> a -> [a]
myReplicate n x = if n <= 0 then [] else x : myReplicate (n-1) x

lucky :: ( Integral a ) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"


sayMe :: ( Integral a ) => a -> String
sayMe 1 = " One ! "
sayMe 2 = " Two ! "
sayMe 3 = " Three !"
sayMe 4 = " Four !"
sayMe 5 = " Five !"
sayMe x = " Not between 1 and 5"

factorial :: ( Integral a ) => a -> a
factorial 0 = 1
factorial n = n * factorial ( n-1 )


addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors ( x1 , y1 ) ( x2 , y2 ) = ( x1 + x2 , y1 + y2)

len' :: (Num b) => [a] -> b
len' [] = 0
len' (_:xs) = 1 + len' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= under  = "Under"
    | bmi <= normal = "Normal"
    | bmi <= over   = "Over"
    | otherwise     = "Way Over"

    where bmi       = weight / height ^ 2
          under     = 18.5
          normal    = 25.0
          over      = 30.0


mx :: (Ord a) => a -> a -> a
mx a b
    | a > b     = a
    | otherwise   = b

comp :: (Ord a) => a -> a -> String
comp a b
    | a > b     = "GT"
    | a == b    = "EQ"
    | a < b     = "LT"


initials :: String -> String -> String
initials (f:_) (l:_) =  [f] ++ "." ++ [l] ++ "."

-- initials firstname lastname = [f] ++ "." ++ [l] ++ "."
    -- where (f:_) = firstname
    --       (l:_) = lastname

    -- OR
    
    -- where f = head firstname
    --       l = head lastname







