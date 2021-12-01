module Common 

import Data.String
import Data.List
import Data.List1

public export
Solution : Type 
Solution = (input : String) -> Either String (String, String)

-- This maps over a list strings and performs some non-total conversion on them 
-- if all lines are successful it returns the list of converted lines, if not 
-- it retuns the index of the first error
export
processLines : (func : String -> Maybe a) -> String -> Either String (List a)
processLines func input = go (lines $ input) func (1, [])
  where 
    go : List String -> (String -> Maybe a) -> (Nat, List a) -> Either String (List a)
    go [] func (count, acc) = Right (reverse acc)
    go [""] func (count, acc) = Right (reverse acc)  -- this might not be needed with the new 'lines' behaviour
    go (x :: xs) func (count, acc) = 
      case (func x) of 
         Just next => go xs func (S count, next :: acc)
         Nothing => Left $ "Error on Line: \{show count}\n"

export 
linesToInts : String -> Either String (List Integer)
linesToInts = processLines parseInteger 

export 
decEither : String -> Dec a -> Either String a 
decEither err (Yes a) = Right a 
decEither err (No a) = Left err

infixl 4 !!
export 
(!!) : List a -> Nat -> Maybe a 
(!!) [] k = Nothing
(!!) (x :: xs) 0 = Just x 
(!!) (x :: xs) (S k) = xs !! k

infixl 4 >|
export 
(>|) : List a -> Integer -> Maybe a 
(>|) [] x = Nothing
(>|) (y :: xs) x =
  case compare x 0 of 
    EQ => Just y 
    GT => xs >| (x - 1)
    LT => Nothing

-- Repeatedly applys a function until it converges 
export
fixedPoint : Eq a => (a -> a) -> a -> a 
fixedPoint f x = 
  let y = f x in 
  if y == x 
     then x 
     else fixedPoint f y 

foldl' : (a -> a -> a) -> List a -> Maybe a 
foldl' f [] = Nothing
foldl' f (x :: xs) = Just $ foldl f x xs

export 
min : Ord a => List a -> Maybe a 
min = foldl' Prelude.min

export 
max : Ord a => List a -> Maybe a 
max = foldl' Prelude.max
