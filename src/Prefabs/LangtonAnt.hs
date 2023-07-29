module Prefabs.LangtonAnt(tAnt) where

import TuringMachine2D
import Tape.Tape2D
import State.Transition2D

import Img.Color

tp1 :: Tape2D RGB
tp1 = beginTape2 white

q1 :: State
q1 = State False "up"

tb :: TransitionTable2D RGB
tb = [
        Transition2D q1 q1 white RRight black,
        Transition2D q1 q1 black RLeft white
    ]

tAnt :: TuringMachine2D RGB
tAnt = beginTuring2 tp1 tb q1