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

isEvil :: Int -> Bool
isEvil a = even $ length $ filter (==1) $ toBin a

-- foldl' (\acc x -> if (x==0) then ([(acc !! 0)+1, acc !! 1]) else ([acc !! 0, (acc !! 1)+1])) [0,0] $ take 20 $ evilNumbers
-- foldl' groupCount [0,0] $ take 20 $ thueMorse
-- groupCount acc x =
--   if (x == 0)
--     then ([(acc !! 0)+1, (acc !! 1)+1])
--     else ([(acc !! 0)+0, (acc !! 1)+1])

-- get fairness of 1s vs 0s in a list. fair means 0.0. unfair favoring 0 is -1.0, unfair favoring 1 is 1.0.
probability :: [Int] -> Bool
probability a = count wins

runTests :: Bool
runTests =
  toBin 0 == [0] &&
  toBin 1 == [1] &&
  toBin 255 == [1,1,1,1, 1,1,1,1] &&
  isEvil 39 &&
  not (isEvil 7) &&
  take 10 evilNumbers == [0, 3, 5, 6, 9, 10, 12, 15, 17, 18]
