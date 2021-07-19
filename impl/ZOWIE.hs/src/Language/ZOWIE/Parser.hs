module Language.ZOWIE.Parser where

import Language.ZOWIE.State


splitLines :: String -> String -> [String]
splitLines [] line = [reverse line]
splitLines ('\n':rest) line = [reverse line] ++ (splitLines rest [])
splitLines (char:rest) line = splitLines rest (char:line)

parseLines [] = []
parseLines (line:lines) =
    case parseLine line of
        Just instr ->
            (instr:parseLines lines)
        Nothing ->
            parseLines lines

parseLine [] = Nothing
parseLine (' ':rest) = parseLine rest
parseLine ('M':'O':'V':rest) = Just $ Mov (Direct 0) (Immediate 0)


parseZOWIE text =
    let
        lines = splitLines text []
        prog = parseLines lines
    in
        prog
