module ZOWIE where

import qualified Data.Map.Strict as Map


type Addr = Integer
type Value = Integer
type Memory = Map.Map Addr Value

readMem mem addr = Map.findWithDefault 0 addr mem
writeMem mem addr value = Map.insert addr value mem

type CPU = Integer

data State = State {
    cpu :: CPU,
    mem :: Memory
} deriving (Show, Ord, Eq)


readAddr :: State -> Addr -> IO Value
readAddr state@State{ cpu=cpu, mem=mem } addr =
    case addr of
        0 -> do            -- TtyRegister
            i <- readLn
            return i
        1 -> return 1      -- BeginTransactionRegister
        2 -> return 2      -- CommitRegister
        3 -> return 3      -- CommitAndRepeatRegister
        4 -> return 4      -- AdditionRegister
        5 -> return 5      -- SubtractionRegister
        6 -> return 6      -- MultiplicationRegister
        7 -> return 7      -- NegationRegister
        x -> return (readMem mem x)


writeAddr :: State -> Addr -> Value -> IO State
writeAddr state@State{ cpu=cpu, mem=mem } addr payload =
    case addr of
        0 -> do               -- TtyRegister
                 print payload
                 return state
        1 -> do               -- BeginTransactionRegister
                 return state{ cpu=(beginTransaction cpu) }
        2 -> let              -- CommitRegister
                 cpu' = if payload > 0 then commit cpu else rollback cpu
             in
                 return state{ cpu=cpu' }
        3 -> let              -- CommitAndRepeatRegister
                 cpu' = if payload > 0 then commitAndRepeat cpu else commit cpu
             in
                 return state{ cpu=cpu' }
        4 -> let              -- AdditionRegister
                 mem' = writeMem mem 8 ((readMem mem 8) + payload)
             in
                 return state{ mem=mem' }
        5 -> let              -- SubtractionRegister
                 mem' = writeMem mem 8 ((readMem mem 8) - payload)
             in
                 return state{ mem=mem' }
        6 -> let              -- MultiplicationRegister
                 mem' = writeMem mem 8 ((readMem mem 8) * payload)
             in
                 return state{ mem=mem' }
        7 -> let              -- NegationRegister
                 mem' = if payload == 0 then writeMem mem 8 1 else writeMem mem 8 0
             in
                 return state{ mem=mem' }
        x -> do
                 return state{ mem=(writeMem mem x payload) }


beginTransaction x = x

commit x = x

commitAndRepeat x = x

rollback x = x
