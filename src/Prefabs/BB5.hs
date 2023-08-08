module Prefabs.BB5(tBB5) where
import Tape.Tape
import State.State
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

tab' :: [Transition Int Direction]
tab' = [
        mkTr qa qb 0 R 1,
        mkTr qa qc 1 L 1,

        mkTr qb qc 0 R 1,
        mkTr qb qb 1 R 1,

        mkTr qc qd 0 R 1,
        mkTr qc qe 1 L 0,

        mkTr qd qa 0 L 1,
        mkTr qd qd 1 L 1,
        
        mkTr qe qH 0 R 1,
        mkTr qe qa 1 L 0
        ]

tBB5 :: TuringMachine Int
tBB5 =  beginTuring tp1 tab' qa