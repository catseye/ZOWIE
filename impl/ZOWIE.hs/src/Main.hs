module Main where

import System.Environment
import System.Exit
import System.IO

import Language.ZOWIE


main = do
    args <- getArgs
    case args of
        ["run", fileName] -> do
            text <- readFile fileName
            putStrLn $ show $ reverse $ run text
            return ()
        _ -> do
            abortWith "Usage: zowie run <carriage-program-text-filename>"

abortWith msg = do
    hPutStrLn stderr msg
    exitWith (ExitFailure 1)
