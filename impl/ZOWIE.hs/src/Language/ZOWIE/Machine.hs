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

run s = s
