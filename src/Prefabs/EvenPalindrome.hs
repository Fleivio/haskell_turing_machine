module Prefabs.EvenPalindrome(tEvenPalindrome) where
import Tape.Tape
import State.Transition
import TuringMachine

tp1 :: Tape Char
tp1 = beginTapeFromList "aabbaa" 'B'

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

tab' :: [Transition Char]
tab' = [
        Transition q0 q7 'B' R 'B',
        Transition q0 q1 'a' R 'B',
        Transition q0 q4 'b' R 'B',
        Transition q1 q1 'a' R 'a',
        Transition q1 q1 'b' R 'b',
        Transition q1 q2 'B' L 'B',
        Transition q2 q3 'a' L 'B',
        Transition q3 q3 'a' L 'a',
        Transition q3 q3 'b' L 'b',
        Transition q3 q0 'B' R 'B',
        Transition q4 q4 'a' R 'a',
        Transition q4 q4 'b' R 'b',
        Transition q4 q5 'B' L 'B',
        Transition q5 q6 'b' L 'B',
        Transition q6 q6 'b' L 'b',
        Transition q6 q6 'a' L 'a',
        Transition q6 q0 'B' R 'B'
        ]

tEvenPalindrome :: TuringMachine Char
tEvenPalindrome =  TM tp1 tab' q0 False 0