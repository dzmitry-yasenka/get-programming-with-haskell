main :: IO ()
main = do
    print "Lesson 05"

ifEven f x =
    if even x
        then f x
        else x

genIfEven :: Integral p => (p -> p) -> p -> p
genIfEven f = (\x -> ifEven f x)

ifEvenInc :: Integer -> Integer
ifEvenInc = genIfEven succ

getRequestURL :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
getRequestURL host apiKey resource id =
    host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
genHostRequestBuilder host =
    \apiKey resource id ->
        getRequestURL host apiKey resource id

exampleUrlBuilder :: [Char] -> [Char] -> [Char] -> [Char]
exampleUrlBuilder = getRequestURL "http://example.com"

myExampleUrlBuilder :: [Char] -> [Char] -> [Char]
myExampleUrlBuilder = exampleUrlBuilder "1337hAsk3ll"