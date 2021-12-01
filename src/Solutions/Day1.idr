module Solutions.Day1

import Common

part1 : Integer -> List Integer -> Integer 
part1 count (first :: second :: rest ) =
    if first > second 
    then part1 count (second :: rest)
    else part1 (count+1) (second :: rest)
part1 count xs = count

part2 : Integer -> Integer -> List Integer -> Integer
part2 count slidingSum (fst :: snd :: trd :: rest) = let
  sum = fst + snd + trd 
  in
  if sum > slidingSum
  then part2 (count+1) sum (snd :: trd :: rest)
  else part2 count sum (snd :: trd :: rest)
part2 count slidingSum other = count

export
run : Solution 
run input = do
  values <- linesToInts input
  let sol1 = show $ part1 0 values
  let sol2 = show $ part2 (-1) 0 values
  Right (sol1, sol2)
