module Prefabs.LangtonAnt(tAnt) where

import TuringMachine2D
import Tape.Tape2D
import State.Transition2D

import Img.Color

tp1 :: Tape2D DColor
tp1 = beginTape2 White

q1 :: State
q1 = State False "up"

tb :: TransitionTable2D DColor
tb = [
        Transition2D q1 q1 White RRight Black,
        Transition2D q1 q1 Black RLeft White
    ]

tAnt :: TuringMachine2D DColor
tAnt = beginTuring2 tp1 tb q1