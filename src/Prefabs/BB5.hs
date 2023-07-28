module Prefabs.BB5(tBB5) where
import Tape.Tape
import State.Transition
import TuringMachine

tp1 :: Tape Int
tp1 = beginTape 0

qa :: State
qa = State False "0"
qb :: State
qb = State False "1"
qc :: State
qc = State False "2"
qd :: State
qd = State False "3"
qe :: State
qe = State False "4"
qH :: State
qH = State True "H"

tab' :: [Transition Int]
tab' = [
        Transition qa qb 0 R 1,
        Transition qa qc 1 L 1,

        Transition qb qc 0 R 1,
        Transition qb qb 1 R 1,

        Transition qc qd 0 R 1,
        Transition qc qe 1 L 0,

        Transition qd qa 0 L 1,
        Transition qd qd 1 L 1,
        
        Transition qe qH 0 R 1,
        Transition qe qa 1 L 0
        ]

tBB5 :: TuringMachine Int
tBB5 =  beginTuring tp1 tab' qa