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
            text <- readFile fileName
            let prog = Parser.parseZOWIE text
            putStrLn $ show $ prog
            return ()
        ["run", fileName] -> do
            text <- readFile fileName
            let prog = Parser.parseZOWIE text
            result <- Machine.loadAndRun prog
            putStrLn $ show $ result
            return ()
        _ -> do
            abortWith "Usage: zowie (parse|run) <zowie-program-filename>"

abortWith msg = do
    hPutStrLn stderr msg
    exitWith (ExitFailure 1)
