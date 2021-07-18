module Language.ZOWIE where

import qualified Data.Map.Strict as Map


type Addr = Integer
type Value = Integer
type Memory = Map.Map Addr Value

readMem mem addr = Map.findWithDefault 0 addr mem
writeMem mem addr value = Map.insert addr value mem

data Register = TtyRegister
              | BeginTransactionRegister
              | CommitRegister
              | CommitAndRepeatRegister
              | AdditionRegister
              | SubtractionRegister
              | MultiplicationRegister
              | NegationRegister
              | RegularRegister Addr

type CPU = Integer

data State = State {
    cpu :: CPU,
    mem :: Memory
} deriving (Show, Ord, Eq)


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
readAddr state@State{ cpu=cpu, mem=mem } addr =
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
writeAddr state@State{ cpu=cpu, mem=mem } addr payload =
    case mapRegister addr of
        TtyRegister -> do
            print payload
            return state
        BeginTransactionRegister ->
            return state{ cpu=(beginTransaction cpu) }
        CommitRegister ->
            return state{ cpu=(if payload > 0 then commit cpu else rollback cpu) }
        CommitAndRepeatRegister ->
            return state{ cpu=(if payload > 0 then commitAndRepeat cpu else commit cpu) }
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


beginTransaction x = x

commit x = x

commitAndRepeat x = x

rollback x = x
