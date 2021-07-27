module Language.ZOWIE.Machine where

import Language.ZOWIE.State
import Language.ZOWIE.Registers (readAddr, writeAddr)


getValue :: State -> Reference -> IO Value
getValue _ (Immediate v) =
    return v
getValue state (Direct addr) =
    readAddr state addr
getValue state (Indirect addr) = do
    addr' <- readAddr state addr
    val <- readAddr state addr'
    return val

setValue :: State -> Reference -> Value -> IO State
setValue _ (Immediate _) _ =
    error "Cannot set the value of an immediate reference"
setValue state (Direct addr) value =
    writeAddr state addr value
setValue state (Indirect addr) value = do
    addr' <- readAddr state addr
    state' <- writeAddr state addr' value
    return state'

applyInstr :: State -> Instruction -> IO State
applyInstr state (Mov dest src) = do
    value <- getValue state src
    state' <- setValue state dest value
    return state'

nth :: [a] -> Integer -> Maybe a
nth [] _ = Nothing
nth (x:xs) 0 = Just x
nth (x:xs) n = nth xs (n-1)

step :: State -> IO (Maybe State)
step state =
    case nth (prog state) (pc state) of
        Just instr -> do
            state' <- applyInstr state instr
            return $ Just state'{ pc=(pc state')+1 }
        Nothing ->
            return Nothing

run :: State -> IO State
run state = do
    result <- step state
    case result of
        Just state' ->
            run state'
        Nothing ->
            return state

loadAndRun prog = run (initState prog)

loadAndRunWithIO getCh putCh prog = run (initState prog){ getCh=getCh, putCh=putCh }
