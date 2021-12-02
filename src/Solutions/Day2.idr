module Solutions.Day2

import Common
import Parser

data Direction = Up Integer 
               | Down Integer
               | Forward Integer 

Position : Type 
Position = (Integer, Integer)

PositionAim : Type 
PositionAim = (Integer, Position)

pairToDirection : (String, Integer) -> Maybe Direction
pairToDirection ("up", num) = Just (Up num)
pairToDirection ("down", num) = Just (Down num)
pairToDirection ("forward", num) = Just (Forward num)
pairToDirection (_, num) = Nothing

parseDirections : Parser (List Direction)
parseDirections = pLines $ mapM pairToDirection (pWord' <&> pInteger)

move : Position -> Direction -> Position
move (x, z) (Up y) = (x, z-y)
move (x, z) (Down y) = (x, z+y)
move (x, z) (Forward y) = (x+y, z)

aimMove : PositionAim -> Direction -> PositionAim
aimMove (aim, (hor, ver)) (Up y) = (aim-y, (hor, ver))
aimMove (aim, (hor, ver)) (Down y) = (aim+y, (hor, ver))
aimMove (aim, (hor, ver)) (Forward y) = (aim, (hor+y, ver + aim*y))

doMoves : (a -> Direction -> a) -> a -> List Direction -> a
doMoves mover start moves = foldl mover start moves

answer : (Integer, Integer) -> Integer
answer (x, y) = x * y

export 
run : Solution 
run input = do 
  moves <- eitherParse parseDirections input
  let sol1 = show . answer $ doMoves move (0,0) moves
  let sol2 = show . answer . snd$ doMoves aimMove (0, (0,0)) moves
  Right (sol1, sol2)
