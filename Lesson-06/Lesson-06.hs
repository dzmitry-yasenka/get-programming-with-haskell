main :: IO ()
main = do
    print "Lesson 06"

isPalindrome :: Eq a => [a] -> Bool
isPalindrome word = word == reverse  word

respond :: Foldable t => t Char -> [Char]
respond phrase =
    if '!' `elem` phrase
        then "Wow!"
        else "uh. okay"

ones :: Num a => Int -> [a]
ones n = take n (cycle [1])

repeat' :: a -> [a]
repeat' n = cycle [n]

subseq :: Int -> Int -> [a] -> [a]
subseq startIndex stopIndex list = drop startIndex (take stopIndex list)

inFirstHalf :: Eq a => a -> [a] -> Bool
inFirstHalf element list =
  let listLength = length list
      halfOfListLength = round (fromIntegral (listLength) / 2)
      firstHalfOfList = take halfOfListLength list
   in 
       elem element firstHalfOfList