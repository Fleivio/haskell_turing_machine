module Prefabs.BB4(tBB4) where
import Tape.Tape
import State.State
import TuringMachine

tp1 :: Tape Int
tp1 = beginTape 0

q0 :: State
q0 = State False "0"
q1 :: State
q1 = State False "1"
q2 :: State
q2 = State False "2"
q3 :: State
q3 = State False "3"
qH :: State
qH = State True "H"
qEnd :: State
qEnd = State False "E"

tab' :: [Transition Int Direction]
tab' = [
        mkTr q0 q1 0 R 1,
        mkTr q0 q1 1 L 1,
        mkTr q1 q0 0 L 1,
        mkTr q1 q2 1 L 0,
        mkTr q2 qH 0 R 1,
        mkTr q2 q3 1 L 1,
        mkTr q3 q3 0 R 1,
        mkTr q3 q0 1 R 0,

        -- force non deterministic behaviour
        mkTr q1 qEnd 1 L 0,
        mkTr q2 qEnd 1 L 1,
        mkTr q3 qEnd 0 R 1,
        mkTr qEnd qEnd 1 R 1,
        mkTr qEnd qEnd 0 R 0
        ]

tBB4 :: TuringMachine Int
tBB4 =  beginTuring tp1 tab' q0