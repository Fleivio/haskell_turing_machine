module Prefabs.LangtonAnt(tAnt) where

import TuringMachine2D
import Tape.Tape2D
import State.Transition2D

data BColor = White | Black deriving (Eq)

instance Show BColor where
    show White = " "
    show Black = "■"

tp1 :: Tape2D BColor
tp1 = beginTape2 White

q1 :: State
q1 = State False "up"

tb :: TransitionTable2D BColor
tb = [
        Transition2D q1 q1 White RRight Black,
        Transition2D q1 q1 Black RLeft White
    ]

tAnt :: TuringMachine2D BColor
tAnt = beginTuring2 tp1 tb q1