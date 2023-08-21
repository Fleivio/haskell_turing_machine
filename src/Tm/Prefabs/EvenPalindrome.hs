module Tm.Prefabs.EvenPalindrome(tEvenPalindrome) where
import Tm.Tape.Tape
import Tm.State.State
import Tm.TuringMachine

tp1 :: Tape Char
tp1 = beginTapeFromList "baabbaab" 'B'

q0 :: State
q0 = State False "0"
q1 :: State
q1 = State False "1"
q2 :: State
q2 = State False "2"
q3 :: State
q3 = State False "3"
q4 :: State
q4 = State False "4"
q5 :: State
q5 = State False "5"
q6 :: State
q6 = State False "6"
q7 :: State
q7 = State True "7"

tab' :: [Transition Char Direction]
tab' = [
        mkTr q0 q7 'B' R 'B',
        mkTr q0 q1 'a' R 'B',
        mkTr q0 q4 'b' R 'B',
        mkTr q1 q1 'a' R 'a',
        mkTr q1 q1 'b' R 'b',
        mkTr q1 q2 'B' L 'B',
        mkTr q2 q3 'a' L 'B',
        mkTr q3 q3 'a' L 'a',
        mkTr q3 q3 'b' L 'b',
        mkTr q3 q0 'B' R 'B',
        mkTr q4 q4 'a' R 'a',
        mkTr q4 q4 'b' R 'b',
        mkTr q4 q5 'B' L 'B',
        mkTr q5 q6 'b' L 'B',
        mkTr q6 q6 'b' L 'b',
        mkTr q6 q6 'a' L 'a',
        mkTr q6 q0 'B' R 'B'
        ]

tEvenPalindrome :: TuringMachine Char
tEvenPalindrome = beginTuring tp1 tab' q0 