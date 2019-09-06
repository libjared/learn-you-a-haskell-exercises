import Data.List (foldl')

-- takes a list of bits in the thue-morse sequence and spits out twice as many.
-- it might not work.
-- morse :: [Int] -> [Int]
-- morse input = foldr foldFunc [] input
--   where
--   foldFunc here acc
--     | here == 0 = 0:1:acc
--     | otherwise = 1:0:acc

-- takes a (uhh, endofunctor, maybe?) function f and creates a function that composes it many times.
-- for example, `composeMany 3 f == f . f . f`
-- composeMany :: Int -> (a -> a) -> a -> a
-- composeMany n f = foldr (.) id (replicate n f)

-- gets unsigned binary as list of int
-- [0] is LSB, [length-1] is MSB
-- 128 -> [1,0,0,0, 0,0,0,0]
toBin :: Int -> [Int]
toBin 0 = [0]
toBin 1 = [1]
toBin a
  | a < 0     = undefined
  | even a    = 0:(toBin $ a`div`2)
  | otherwise = 1:(toBin $ (a-1)`div`2)

evilNumbers :: [Int]
evilNumbers = filter isEvil $ [0..]

thueMorse :: [Int]
thueMorse = map (\x -> if isEvil x then 0 else 1) [0..]

-- unfortunately iterate is worthless
-- thueMorse' :: [Int]
-- thueMorse' = iterate thueMorseIteration' [0]
--
-- thueMorseIteration' :: [Int] -> [Int]
-- thueMorseIteration' a = a ++ flippy
--   where flippy = map notButItsAnInt a
--         notButItsAnInt x
--           | x == 0    = 1
--           | otherwise = 0

thueMorseFormal :: [Int]
thueMorseFormal = map thueMorseFormalAt [0..]

thueMorseFormalAt :: Int -> Int
thueMorseFormalAt 0 = 0
thueMorseFormalAt a
  | even a = thueMorseFormalAt (a `div` 2)
  | odd a  = flippy $ thueMorseFormalAt ((a-1) `div` 2)
    where flippy b
                    | b == 0    = 1
                    | otherwise = 0

thueMorseCustom :: [Int]
thueMorseCustom = [0]

thueMorseCustomAt :: Int -> Int
thueMorseCustomAt 0 = 0

isEvil :: Int -> Bool
isEvil a = even $ length $ filter (==1) $ toBin a

-- creates an infinite sequence from (a function that takes a sequence and
-- produces a sequence), and initial seed list.  this only works if that
-- function does not change what's in the sequence already, but just appends.
--
-- actually no, we can't enforce that, so here:
-- creates an infinite sequence from (a function that takes a list and produces
-- another list to be appended), and the initial seed list.
--
-- example: eateration (\s -> [(last $ init s) + (last s)]) [0,1]
-- [0,1,1,2,3,5,8...]
-- eateration :: ([Int] -> [Int]) -> [Int] -> [Int]
-- eateration createNextChunk seed =
--
-- I feel like I can use a fold for this.
-- I might be able to, but I expect it to involve nested lists. since it's
-- infinite, IDK if I can get away with that.
-- it would have to be a right fold since I am working upwards, and the
-- recursion (is it recursion?) would be infinite on purpose

-- get fairness of 1s vs 0s in a list. fair means 0.0. unfair favoring 0 is -1.0, unfair favoring 1 is 1.0.
--probability :: [Int] -> Float
probability a = (wins/total) - (losses/total)
  where total = fromIntegral $ length a
        wins  = fromIntegral $ length $ filter (==1) a
        losses= fromIntegral $ length $ filter (==0) a

runTests :: Bool
runTests =
  toBin 0 == [0] &&
  toBin 1 == [1] &&
  toBin 255 == [1,1,1,1, 1,1,1,1] &&
  isEvil 39 &&
  not (isEvil 7) &&
  take 10 evilNumbers == [0, 3, 5, 6, 9, 10, 12, 15, 17, 18] &&
  take 10 thueMorse == [0, 1, 1, 0, 1, 0, 0, 1, 1, 0] &&
  probability [0,0,0] == -1.0 &&
  probability [1,1,1] == 1.0 &&
  probability [1,0,1] == (2/3)-(1/3) &&
  take 500 thueMorse == take 500 thueMorseFormal
