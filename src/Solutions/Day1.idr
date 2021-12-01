module Solutions.Day1

import Common

part1 : Integer -> List Integer -> Integer 
part1 count (first :: next@( second :: rest )) = let
    count = if first < second then count + 1 else count in
    part1 count next
part1 count xs = count

part2 : Integer -> Integer -> List Integer -> Integer
part2 count slidingSum (fst :: next@(snd :: trd :: rest)) = let
  sum = fst + snd + trd 
  count = if sum > slidingSum then count + 1 else count in
  part2 count sum next
part2 count slidingSum other = count - 1

export
run : Solution 
run input = do
  values <- linesToInts input
  let sol1 = part1 0 values            -- 1722
  let sol2 = part2 0 0 values       -- 1748
  Right (show sol1, show sol2)
