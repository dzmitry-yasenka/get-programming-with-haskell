main :: IO ()
main = do
    print "Lesson 07"

myGCD :: Integral t => t -> t -> t
myGCD a b =
    if remainder == 0
        then b
        else myGCD b remainder
    where
        remainder = a `mod` b

sayAmount :: (Eq a, Num a) => a -> [Char]
sayAmount n = case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    n -> "a bunch"

sayAmount' :: (Eq a, Num a) => a -> [Char]
sayAmount' 0 = "zero"
sayAmount' 1 = "one"
sayAmount' 2 = "two"
sayAmount' n = "a bunch"

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

myHead :: [p] -> p
myHead [] = error "No head for an empty list"
myHead (x:xs) = x

myTail :: [a] -> [a]
myTail [] = []
myTail (_:xs) = xs

gcd'' :: Int -> Int -> Int
gcd'' x 0 = abs x
gcd'' x y = gcd'' b (mod a b)
   where a = abs x
         b = abs y