module Tm.Tm(TuringMachine(..), mkTm) where

-- import Tape.Tape
import State.State

type Tape a dim = (a, dim)

data TuringMachine a dim = TM {
    tape :: Tape a dim,
    transitionTable :: TransitionTable a dim,
    currentState :: State,
    halt :: Bool,
    count :: Int
}

instance (Show a) => Show (TuringMachine a d) where
    show (TM _ _ st h c) =      "\nSteps: " ++ show c
                               ++ "\nAccepted: " ++ show (h && isAccept st)

mkTm :: Tape a d -> TransitionTable a d -> State -> TuringMachine a d
mkTm tape transTable state = TM tape transTable state False 0

-- tmPerformAction :: TuringMachine a d -> Action a d -> TuringMachine a d
-- tmPerformAction tm Fail = tm { halt = True }
-- tmPerformAction tm@(TM t _ _ _ c) (Action nxt wChar dir)
--                     = tm {
--                         tape = tapeShift (tapeWrite t wChar) dir,
--                         currentState = nxt,
--                         count = c + 1}
