{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
main :: IO ()
main = do
    print "Lesson 03"

maxBetweenSumOfSquaresAndSquareOfSum :: (Ord p, Num p) => p -> p -> p
maxBetweenSumOfSquaresAndSquareOfSum x y = 
    if sumOfSquares > squareOfSum
        then sumOfSquares
        else squareOfSum
    where
        sumOfSquares = x^2 + y^2
        squareOfSum = (x + y)^2

maxBetweenSumOfSquaresAndSquareOfSum' :: (Num a, Ord a) => a -> a -> a
maxBetweenSumOfSquaresAndSquareOfSum' x y =
    (\ sumOfSquares squareOfSum ->
        if sumOfSquares > squareOfSum
            then sumOfSquares
            else squareOfSum ) (x^2 + y^2) ((x + y)^2)

maxBetweenSumOfSquaresAndSquareOfSum'' :: (Ord p, Num p) => p -> p -> p
maxBetweenSumOfSquaresAndSquareOfSum'' x y = 
    let sumOfSquares = x^2 + y^2
        squareOfSum = (x + y)^2
    in
        if sumOfSquares > squareOfSum
            then sumOfSquares
            else squareOfSum

-- Not working due to let limitations
-- counter :: Num a => p -> a
-- counter x = 
--     let x = x + 1
--     in 
--         let x = x + 1
--         in 
--             x

--lambda analogue which is working
counter x = (\x -> x + 1) ((\x -> x + 1) x)