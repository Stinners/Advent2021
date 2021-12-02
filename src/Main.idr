module Main 

import System 
import System.File 
import Control.Monad.Either
import Data.List

import Common
import Parser

import Solutions.Day1
import Solutions.Day2

data Env = Run | Test

-- Read the contents of the input file as a string
getInput : Env -> (day : String) -> EitherT String IO String
getInput env day = do
  let filename = case env of 
                      Run  => "./inputs/day" ++ day ++".txt"
                      Test => "./inputs/day" ++ day ++"_test.txt"
  input <- readFile filename
  case input of 
    Right output => right output 
    Left err => left . show $ "Could not find: \{filename}"

getSolution : (day : String) -> EitherT String IO Solution
getSolution day = case day of 
     "1" => right Day1.run
     "2" => right Day2.run
     _ => left "No solution found for this day"

displaySolution : (solution : Either String (String, String)) -> String
displaySolution (Left err) = err
displaySolution (Right (part1, part2)) =
  "\nPart1:\n" ++ part1 ++ "\n\nPart2:\n" ++ part2 ++ "\n"

runProgram : Env -> (day : String) -> EitherT String IO String
runProgram env day = do
  input <- getInput env day        
  solution <- getSolution day
  pure . displaySolution . solution $ input 

getDayNumber : IO (Maybe String)
getDayNumber = map (tail' >=> head') getArgs 


doProgram : Show day => Env -> day -> IO ()
doProgram env day = do
  Right result <- runEitherT (runProgram env (show day))
    | Left err => putStrLn err
  putStr result

-- This is extracted here to make running from the repl easier
run : Show day => day -> IO ()
run = doProgram Run

-- This is extracted here to make running from the repl easier
test : Show day => day -> IO ()
test = doProgram Test

main : IO ()
main = do 
  Just day <- getDayNumber 
    | Nothing => putStrLn "Please enter a day number"
  run day
