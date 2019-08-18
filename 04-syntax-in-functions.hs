-- This function should print a single digit number as English text, or "unknown" if it's out of the range 0-9
englishDigit :: Int -> String
englishDigit 0 = "zero"
englishDigit 1 = "one"
englishDigit 2 = "two"
englishDigit 3 = "three"
englishDigit 4 = "four"
englishDigit 5 = "five"
englishDigit 6 = "six"
englishDigit 7 = "seven"
englishDigit 8 = "eight"
englishDigit 9 = "nine"
englishDigit x = "unknown"

-- given a tuple, divide fst by snd, using pattern matching. 
-- it should return undefined for division by zero
--divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, 0) = undefined
divTuple (x, y) = x/y

-- if the first three numbers in a list are all zero, return True
threeZeroList :: [Int] -> Bool
threeZeroList (a:b:c:_) = all (==0) [a,b,c]
threeZeroList [x] = False

runTests :: Bool
runTests =
    englishDigit 0 == "zero" &&
    englishDigit 5 == "five" &&
    englishDigit 10 == "unknown" &&
    divTuple (4,2) == 2 &&
    divTuple (0,2) == 0 &&
    --undefined is an error, I can't test against it
    --divTuple (2,0) == undefined
    not (threeZeroList [0]) &&
    not (threeZeroList [0,0,5]) &&
    threeZeroList [0,0,0] &&
    threeZeroList [0,0,0,5]
