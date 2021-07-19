module Language.ZOWIE.Parser where

import Text.ParserCombinators.Parsec

import Language.ZOWIE.State


splitLines :: String -> String -> [String]
splitLines [] line = [reverse line]
splitLines ('\n':rest) line = [reverse line] ++ (splitLines rest [])
splitLines (char:rest) line = splitLines rest (char:line)

--
-- The grammar of a line is
--
-- Line    ::= Comment | "MOV" Operand "," Operand [Comment].
-- Operand ::= "R[R" Number "]" | "R" Number | Number.
-- Comment ::= ";" anything.
--

zowieLine = commentLine <|> instrLine

commentLine :: Parser (Maybe Instruction)
commentLine = do
    spaces
    string ";"
    many $ satisfy (\x -> x /= '\n')
    return Nothing

instrLine :: Parser (Maybe Instruction)
instrLine = do
    spaces
    string "MOV"
    dest <- operand
    spaces
    string ","
    src <- operand
    optional commentLine
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

number = do
    c <- digit
    cs <- many digit
    num <- return (read (c:cs) :: Integer)
    return num


parseLines [] = []
parseLines (line:lines) =
    case parse zowieLine "" line of
        Left err ->
            parseLines lines
        Right result ->
            case result of
                Just instr ->
                    (instr:parseLines lines)
                Nothing ->
                    parseLines lines

parseZOWIE text =
    let
        lines = splitLines text []
        prog = parseLines lines
    in
        prog
