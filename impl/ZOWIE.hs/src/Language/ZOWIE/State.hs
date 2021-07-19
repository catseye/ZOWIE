module Language.ZOWIE.State where

import qualified Data.Map.Strict as Map


type Addr = Integer
type Value = Integer

type Memory = Map.Map Addr Value

data Reference = Immediate Value
               | Direct Addr
               | Indirect Addr
    deriving (Show, Ord, Eq)

data Instruction = Mov Reference Reference
    deriving (Show, Ord, Eq)

data State = State {
    pc :: Addr,
    mem :: Memory,
    prog :: [Instruction],
    saved :: Maybe State
} deriving (Show, Ord, Eq)


initState :: [Instruction] -> State
initState prog =
    State{
        pc=0,
        mem=Map.empty,
        prog=prog,
        saved=Nothing
    }

readMem mem addr = Map.findWithDefault 0 addr mem
writeMem mem addr value = Map.insert addr value mem

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
