import Data.List

main :: IO ()
main = do
    print "Lesson 04"

ifEvenInc :: Integral p => p -> p
ifEvenInc n = if even n
    then n + 1
    else n

ifEvenDouble :: Integral p => p -> p
ifEvenDouble n = if even n
    then n * 2
    else n

ifEvenSquare :: Integral p => p -> p
ifEvenSquare n = if even n
    then n ^ 2
    else n

ifEven :: Integral p => (p -> p) -> p -> p
ifEven f n = if even n
    then f n
    else n

inc :: Num a => a -> a
inc n = n + 1

double :: Num a => a -> a
double n = n * 2

square :: Num a => a -> a
square n = n ^ 2

ifEvenInc' :: Integral p => p -> p
ifEvenInc' = ifEven inc

ifEvenDouble' :: Integral p => p -> p
ifEvenDouble' = ifEven double

ifEvenSquare' :: Integral p => p -> p
ifEvenSquare' = ifEven square

names =
  [ ("Ian", "Curtis"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook"),
    ("Albert", "Hook"),
    ("Stephen", "Morris"),
    ("Ben", "Curtis")
  ]

-- example from the book
-- sortBy compareNames names
compareNames :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Ordering
compareNames name1 name2 =
    if lastName1 < lastName2
        then LT
        else if lastName1 > lastName2
            then GT
            else if firstName1 < firstName2
                then LT
                else if firstName1 > firstName2
                    then GT
                    else EQ
    where
        lastName1 = snd name1
        lastName2 = snd name2
        firstName1 = fst  name1
        firstName2 = fst name2

-- better implementation with guards.
-- sortBy compareNames' names
compareNames' :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Ordering
compareNames' name1 name2
  | lastName1 < lastName2 = LT
  | lastName1 > lastName2 = GT
  | firstName1 < firstName2 = LT
  | firstName1 > firstName2 = GT
  | otherwise = EQ
  where
      lastName1 = snd name1
      lastName2 = snd name2
      firstName1 = fst name1
      firstName2 = fst name2

compareNames'' :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Ordering
compareNames'' name1 name2 =
    if secondNamesComparisonResult == EQ
        then firstNamesComparisonResult
        else secondNamesComparisonResult
    where
        secondNamesComparisonResult = compare (snd name1) (snd name2)
        firstNamesComparisonResult = compare (fst name1) (fst name2)

sfOffice :: ([Char], [Char]) -> [Char]
sfOffice name = if lastName < "L"
    then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
 where 
    lastName = snd name
    nameText = fst name ++ " " ++ lastName

nyOffice :: ([Char], [Char]) -> [Char]
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = fst name ++ " " ++ snd name

renoOffice :: ([Char], [Char]) -> [Char]
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
    where nameText = snd name

dcOffice :: ([Char], [Char]) -> [Char]
dcOffice name = nameText ++ " - DC"
    where nameText = fst name ++ " " ++ snd name ++ ".Esq"

getLocationFunction :: [Char] -> ([Char], [Char]) -> [Char]
getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "dc" -> dcOffice
  _ -> (\name -> fst name ++ " " ++ snd name)

addressLetter :: ([Char], [Char]) -> [Char] -> [Char]
addressLetter name location = locationFunction name
    where locationFunction = getLocationFunction location
