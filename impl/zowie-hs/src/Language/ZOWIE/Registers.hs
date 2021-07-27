module Language.ZOWIE.Registers where

import Data.Char (chr)

import Language.ZOWIE.State


data Register = TtyRegister
              | BeginTransactionRegister
              | CommitRegister
              | CommitAndRepeatRegister
              | AdditionRegister
              | SubtractionRegister
              | MultiplicationRegister
              | NegationRegister
              | RegularRegister Addr


mapRegister 0 = TtyRegister
mapRegister 1 = BeginTransactionRegister
mapRegister 2 = CommitRegister
mapRegister 3 = CommitAndRepeatRegister
mapRegister 4 = AdditionRegister
mapRegister 5 = SubtractionRegister
mapRegister 6 = MultiplicationRegister
mapRegister 7 = NegationRegister
mapRegister x = RegularRegister x

readAddr :: State -> Addr -> IO Value
readAddr state@State{ mem=mem } addr =
    case mapRegister addr of
        TtyRegister -> do
            i <- readLn
            return i
        BeginTransactionRegister -> return 1
        CommitRegister           -> return 2
        CommitAndRepeatRegister  -> return 3
        AdditionRegister         -> return 4
        SubtractionRegister      -> return 5
        MultiplicationRegister   -> return 6
        NegationRegister         -> return 7
        RegularRegister x        -> return (readMem mem x)

writeAddr :: State -> Addr -> Value -> IO State
writeAddr state@State{ mem=mem } addr payload =
    case mapRegister addr of
        TtyRegister -> do
            putChar $ chr $ fromIntegral payload
            return state
        BeginTransactionRegister ->
            return $ beginTransaction state
        CommitRegister ->
            return $ if payload > 0 then commit state else rollback state
        CommitAndRepeatRegister ->
            return $ if payload > 0 then commitAndRepeat state else commit state
        AdditionRegister ->
            return state{ mem=(writeMem mem 8 ((readMem mem 8) + payload)) }
        SubtractionRegister ->
            return state{ mem=(writeMem mem 8 ((readMem mem 8) - payload)) }
        MultiplicationRegister ->
            return state{ mem=(writeMem mem 8 ((readMem mem 8) * payload)) }
        NegationRegister ->
            return state{ mem=(writeMem mem 8 (if payload == 0 then 1 else 0)) }
        RegularRegister x ->
            return state{ mem=(writeMem mem x payload) }
