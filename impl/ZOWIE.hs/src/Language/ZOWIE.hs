module Language.ZOWIE where

import qualified Data.Map.Strict as Map


type Addr = Integer
type Value = Integer
type Memory = Map.Map Addr Value

readMem mem addr = Map.findWithDefault 0 addr mem
writeMem mem addr value = Map.insert addr value mem

data Instruction = Immediate Addr Value
                 | Direct Addr Addr
                 | Indirect Addr Addr Value
    deriving (Show, Ord, Eq)

data State = State {
    pc :: Addr,
    mem :: Memory,
    prog :: [Instruction],
    saved :: Maybe State
} deriving (Show, Ord, Eq)

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
            print payload
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


beginTransaction :: State -> State
beginTransaction state@State{} =
    state{ saved=(Just state) }


rollback :: State -> State
rollback state@State{ pc=pc, saved=(Just previous) } =
    previous{ pc=pc }


commit :: State -> State
commit state@State{ saved=(Just previous) } =
    state{ saved=(saved previous) }


commitAndRepeat :: State -> State
commitAndRepeat state@State{ saved=(Just previous) } =
    state{ pc=((pc previous) - 1) }
