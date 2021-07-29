module Main where

import System.Environment
import System.Exit
import System.IO

import qualified Language.ZOWIE.Parser as Parser
import qualified Language.ZOWIE.Machine as Machine


main = do
    args <- getArgs
    case args of
        ["parse", fileName] -> do
            prog <- loadSource fileName
            putStrLn $ show $ prog
            return ()
        ["run", fileName] -> do
            prog <- loadSource fileName
            Machine.loadAndRun prog
            return ()
        _ -> do
            abortWith "Usage: zowie-hs (parse|run) <zowie-program-filename>"

loadSource fileName = do
    text <- readFile fileName
    case Parser.parseZOWIE text of
        Right prog -> do
            return prog
        Left error ->
            abortWith $ show error

abortWith msg = do
    hPutStrLn stderr msg
    exitWith $ ExitFailure 1
