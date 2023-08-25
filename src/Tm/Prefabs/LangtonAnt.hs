module Tm.Prefabs.LangtonAnt(tAnt) where

import Tm.TuringMachine2D
import Tm.Tape.ExpTape2
import Tm.Tape.Basic.Rotation
import Tm.State.State

import Img.Color

tp1 :: Tape2D RGB
tp1 = mkTape2 white

q1 :: State
q1 = State False "up"

tb :: TransitionTable RGB Rotation
tb = [
        mkTr q1 q1 white RRight black,
        mkTr q1 q1 black RLeft white
    ]

tAnt :: TuringMachine2D RGB
tAnt = beginTuring2 tp1 tb q1