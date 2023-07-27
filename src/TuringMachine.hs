module TuringMachine(TuringMachine(..), tmRun) where
import Tape.Tape
import State.Transition

import Debug.Trace

data TuringMachine a = TM {
        tape :: Tape a,
        transitionTable :: TransitionTable a,
        currentState :: State,
        halt :: Bool
    }

instance (Show a) => Show (TuringMachine a) where
    show tm = show (tape tm) ++ " " ++ show (currentState tm)

tmRun :: (Eq a, Show a) => TuringMachine a -> TuringMachine a
tmRun tm@(TM _ _ _ True) = tm 
tmRun tm@(TM t trtab s _) =
    case action of 
        Fail -> tm{halt = True}
        Action nxt wChar dir -> trace (show tm) 
            (tmRun tm { 
                    tape = tapeShift (tapeWrite t wChar) dir, 
                    currentState = nxt
                    })
    where rChar = tapeRead t
          action = nextAction rChar s trtab