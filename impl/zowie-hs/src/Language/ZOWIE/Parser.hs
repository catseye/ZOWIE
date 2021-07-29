module Language.ZOWIE.Parser where

import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec
  (
     many, many1, string, satisfy, Parser, (<|>), digit, newline, optional, try, parse
  )

import Language.ZOWIE.State


--
-- The grammar of a line is
--
-- Line    ::= Comment | "MOV" Operand "," Operand [Comment].
-- Operand ::= "R[R" Number "]" | "R" Number | Number.
-- Comment ::= ";" anything.
--

zowie = many1 (commentLine <|> instrLine)

comment = do
    spaces
    string ";"
    many $ satisfy (\x -> x /= '\n')

commentLine :: Parser (Maybe Instruction)
commentLine = do
    optional comment
    newline
    return Nothing

instrLine :: Parser (Maybe Instruction)
instrLine = do
    spaces
    string "MOV"
    dest <- operand
    spaces
    string ","
    src <- operand
    optional comment
    newline
    return $ Just $ Mov dest src

operand = do
    spaces
    r <- (try indirect) <|> (try direct) <|> immediate
    return r

indirect = do
    string "R[R"
    n <- number
    string "]"
    return $ Indirect n

direct = do
    string "R"
    n <- number
    return $ Direct n

immediate = do
    n <- number
    return $ Immediate n

-- ..................................................... --

number = do
    c <- digit
    cs <- many digit
    num <- return (read (c:cs) :: Integer)
    return num

spaces = many $ satisfy (\x -> x `elem` [' ', '\t'])

-- ..................................................... --

parseZOWIE text =
    case parse zowie "" (text ++ "\n") of
        Left err ->
            Left err
        Right maybes ->
            Right (catMaybes maybes)
