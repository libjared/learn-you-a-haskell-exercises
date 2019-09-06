import Data.List (findIndex)
import Data.Maybe (isJust, fromJust)

-- implementation of interesting vim commands, usually against a string

-- navigates to the matching brace. if the character under the cursor does not
-- contain a brace, scans forward until a brace is found and go from there. if
-- the scan finds no braces at all, doesn't move the cursor.
-- valid brace types are <[{()}]>
--
-- params:
--  str - string. text body to search through
--  start - int. starting cursor position
-- returns:
--  int - location of matching brace. if none, returns value of start
percent :: [Char] -> Int -> Int
percent str start = undefined

seekToNextBrace :: [Char] -> Int -> Maybe Int
seekToNextBrace str start = maybeAdd foundBrace start
  where didFind    = isJust foundBrace
        foundBrace = findIndex (`elem` validBraces) cutString
        cutString  = drop start str

maybeAdd ma b = if (isJust ma) then (Just (fromJust ma + b)) else Nothing

validBracesStart = "<[{("
validBracesEnd = ">]})"
validBraces = validBracesStart ++ validBracesEnd

runTests :: Bool
runTests =
  seekToNextBrace "a[b]cd" 0 == Just 1 &&
  seekToNextBrace "a[b]cd" 1 == Just 1 &&
  seekToNextBrace "a[b]cd" 2 == Just 3 &&
  seekToNextBrace "a[b]cd" 3 == Just 3 &&
  seekToNextBrace "a[b]cd" 4 == Nothing
  -- percent "{[<>]}" 1 == 4
