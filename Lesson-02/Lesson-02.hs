main :: IO ()
main = do
    print "Lesson 02"

simple :: p -> p
simple x = x

definitionX :: Integer
definitionX = 2
definitionY :: Integer
definitionY = 3

calculateChange :: (Ord p, Num p) => p -> p -> p
calculateChange given owed = if change > 0
        then change
        else 0
    where change = given - owed

doublePlusTwo :: Num a => a -> a
doublePlusTwo x = doubleX + 2
    where doubleX = x * 2

inc :: Num a => a -> a
inc n = n + 1

double :: Num a => a -> a
double n = n * 2

square :: Num a => a -> a
square n = n ^ 2

f :: Integral a => a -> a
f n = if even n
    then n - 2
    else 3 * n + 1

