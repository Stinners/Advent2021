module Parser

import Data.String
import Data.List 

public export 
Input : Type 
Input = List Char 

public export 
data ParseResult a = Done (a, Input) | Error String

-- To make a parser we need to take a function to maps from input to 
-- A parse result and feed it into the AParser constuctor 
public export 
data Parser : Type -> Type where 
  AParser : (Input -> ParseResult a) -> Parser a

%name Parser parser, parser2 

-- To actually apply a parser we need to call the parse function
export 
parse : Parser a -> Input -> ParseResult a
parse (AParser p) input = p input 

export 
doParse : Parser a -> String -> ParseResult a 
doParse parser str = parse parser (unpack str)

export 
eitherParse : Parser a -> String -> Either String a 
eitherParse parser input = 
  case doParse parser input of 
    Done (a, rest) => Right a 
    Error str => Left str

-- This is a more general form of map, it takes a parser, applies it 
-- and the maps over both the result and the remaining input
export 
continue : Parser a -> ((a, Input) -> ParseResult b) -> Parser b 
continue parser f = AParser $ \input => 
    case parse parser input of 
      Error str => Error str 
      Done a => f a 

public export 
Functor ParseResult where 
  map _ (Error str) = Error str 
  map f (Done (a, input)) = Done (f a, input)

public export
Functor Parser where 
  -- map : (a -> b) -> Parser a -> Parser b
  map f parser = AParser $ map f . parse parser

public export
Applicative Parser where 
  (<*>) parser1 parser2 =
    AParser $ \input => 
      case parse parser1 input of 
        Error err => Error err 
        Done (a, rest) => parse (map a parser2) rest

  pure a = AParser \input => Done (a, input)

public export
Monad Parser where 
  -- (>>=) : Parser a -> (a -> Parser b) -> Parser b
  (>>=) parser f = continue parser (\(a, rest) => parse (f a) rest) 

-- Defining Basic parsers 

export
end : ParseResult a 
end = Error "End of Input"

export
failure : Parser a 
failure = AParser $ \input => Error "Always fails"

export
parseError : String -> Parser a 
parseError err = AParser $ \input => Error err

export
pToken : Parser Char 
pToken = AParser $ \input => 
  case input of 
    [] => end 
    (c :: cs) => Done (c, cs)

export
validate : (a -> Bool) -> Parser a -> Parser a
validate f parser = do 
  result <- parser 
  if f result 
    then pure result
    else parseError "Invalid Parse"

export
pBool : (Char -> Bool) -> Parser Char
pBool f = validate f pToken

-- parse a specific word 
export 
pTag : String -> Parser String 
pTag tag = AParser $ go (unpack tag) []
  where 
    go : List Char -> List Char -> Input -> ParseResult String
    go [] acc input = Done (pack . reverse $ acc, input)
    go (c :: cs) acc [] = end
    go (c :: cs) acc (n :: rest) = 
      if c == n then go cs (n :: acc) rest
      else Error "Did not match tag"


-- Apply a parser N times 
-- This should return a Vect 
-- There must be a better way to do this 
export
pN : Parser a -> Nat -> Parser (List a)
pN parser n = go parser n []
  where 
    go : Parser a -> Nat -> List a -> Parser (List a)
    go parser 0 xs = failure
    go parser (S k) acc = do
      parsed <- parser 
      let newAcc = parsed :: acc
      case k of 
        0 => pure $ reverse newAcc
        k => go parser k newAcc


export
pChar : Char -> Parser Char 
pChar c = pBool (==c)

export 
pDigit : Parser Char 
pDigit = pBool isDigit

export
many : Parser a -> Parser (List a)
many parser = AParser $ map reverse . go []
  where 
    go : List a -> Input -> ParseResult (List a)
    go acc input = 
      case parse parser input of 
        Done (parsed, rest) => go (parsed :: acc) rest 
        Error str => Done (acc, input)

export
manyString : Parser Char -> Parser String 
manyString parser = map pack (many parser)

export
or : Parser a -> Parser a -> Parser a 
or parser1 parser2 = AParser $ \input =>
  case parse parser1 input of 
    Done (result, rest) => Done (result, rest)
    Error _ => parse parser2 input

infixr 4 <^>
export 
(<^>) : Parser a -> Parser a -> Parser a
(<^>) x y = or x y

export
while : (Char -> Bool) -> Parser String 
while f = manyString (pBool f)

export
until : (Char -> Bool) -> Parser String 
until f = while (not . f)

export
pWhiteSpace : Parser String 
pWhiteSpace = while isSpace

export
-- Parse until it encounters whitespace 
-- Doesn't consume the whitespace
pWord : Parser String 
pWord = until isSpace

export
mapM : (a -> Maybe b) -> Parser a -> Parser b 
mapM f parser = do
  parsed <- parser 
  case f parsed of 
    Nothing => parseError "Invalid mapM"
    Just a => pure a 

export 
pInteger : Parser Integer 
pInteger = mapM parseInteger (while validChar)
  where validChar : Char -> Bool 
        validChar '+' = True
        validChar '-' = True
        validChar c = isDigit c

export
pNat : Parser Nat 
pNat = map stringToNatOrZ (while isDigit)

export
andThen : Parser a -> Parser b -> Parser (a,b)
andThen parser1 parser2 = do
  parsed1 <- parser1
  parsed2 <- parser2
  pure (parsed1, parsed2)

infixl 3 <&>
export 
(<&>) : Parser a -> Parser b -> Parser (a,b)
(<&>) x y = andThen x y 

export
thenDrop : Parser a -> Parser b -> Parser a 
thenDrop parser1 parser2 = map fst (andThen parser1 parser2)

infixl 3 :>
export 
(:>) : Parser a -> Parser b -> Parser a 
(:>) x y = thenDrop x y 

export 
sepBy : Parser a -> Parser b -> Parser (List a)
sepBy parser1 parser2 = do 
  most <- many (thenDrop parser1 parser2)
  AParser $ (\input => 
      case parse parser1 input of 
        Done (result, rest) => Done (most ++ [result], rest)
        Error str => Done (most, input))

export 
-- Matches the end of the file
theEnd : Parser ()
theEnd = AParser $ \input => 
  if input == [] then Done ((), [])
  else Error "Expected End of File"

export
-- Parses a word and consume whitespace
pWord' : Parser String 
pWord' = pWord :> pWhiteSpace

export 
ignore : Parser a -> Parser () 
ignore parser = map (\i => ()) parser

export 
oneOf : List (Parser a) -> Parser a
oneOf parsers = foldl (<^>) (parseError "No opertion found") parsers

export 
ignoreThen : Parser a -> Parser b -> Parser b 
ignoreThen parser1 parser2 = map snd (parser1 <&> parser2)



-- Parses a line as text and consumes the newline 
-- TODO make this work on windows style newlines
export 
line : Parser String 
line = (until (=='\n')) `thenDrop` (pChar '\n')

export 
pLines : Parser a -> Parser (List a)
pLines parser = sepBy parser line

infixl 3 <: 
export 
(<:) : Parser a -> Parser b -> Parser b
(<:) x y = ignoreThen x y 
