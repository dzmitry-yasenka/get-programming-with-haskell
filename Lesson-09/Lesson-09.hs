main = do
    print "Lesson 09"

add3ToAll :: Num a => [a] -> [a]
add3ToAll [] = []
add3ToAll (x : xs) = (3 + x) : add3ToAll xs

multiplyAllBy3 :: Num a => [a] -> [a]
multiplyAllBy3 [] = []
multiplyAllBy3 (x : xs) = (3 * x) : multiplyAllBy3 xs

squareAll :: Num a => [a] -> [a]
squareAll [] = []
squareAll (x : xs) = (x ^ 2) : squareAll xs

map' :: (t -> a) -> [t] -> [a]
map' f [] = []
map' f (x : xs) = (f x) : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' predicate [] = []
filter' predicate (x : xs) =
  if predicate x
    then x : filter' predicate xs
    else filter' predicate xs

remove' :: (a -> Bool) -> [a] -> [a]
remove' predicate [] = []
remove' predicate (x : xs) =
  if predicate x
    then remove' predicate xs
    else x : remove' predicate xs

myProduct :: (Foldable t, Num b) => t b -> b
myProduct xs = foldl (*) 1 xs

sumOfSquares :: Num b => [b] -> b
sumOfSquares xs = foldl (+) 0 (map (^ 2) xs)

elem' :: Eq a => a -> [a] -> Bool
elem' item list =
  if length (remove' (/= item) list) == 0
    then False
    else True

harmonic :: (Fractional a, Enum a) => Int -> a
harmonic n = sum (take n (map (1 /) [1 ..]))