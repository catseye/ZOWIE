module Main where

import System.Environment
import System.Exit
import System.IO

import qualified Language.ZOWIE.Machine as Machine


main = do
    args <- getArgs
    case args of
        ["run", fileName] -> do
            text <- readFile fileName
            result <- Machine.loadAndRun text
            putStrLn $ show $ result
            return ()
        _ -> do
            abortWith "Usage: zowie run <carriage-program-text-filename>"

abortWith msg = do
    hPutStrLn stderr msg
    exitWith (ExitFailure 1)
