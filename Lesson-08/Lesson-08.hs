main :: IO ()
main = do
    print "Lesson 08"

myLength :: Num p => [a] -> p
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myTake :: (Eq a1, Num a1) => a1 -> [a2] -> [a2]
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x:rest
    where rest = myTake (n-1) xs

myCycle :: [a] -> [a]
myCycle (first:rest) = first:myCycle (rest ++ [first])

ackermann :: (Num a, Num t, Eq a, Eq t) => a -> t -> t
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m -1) 1
ackermann m n = ackermann (m -1) (ackermann m (n -1))

drop' :: (Eq a1, Num a1) => a1 -> [a2] -> [a2]
drop' 0 list = list
drop' _ [] = []
drop' n (x : xs) = rest
  where
    rest = drop' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

fib :: (Eq a, Num a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n -2)