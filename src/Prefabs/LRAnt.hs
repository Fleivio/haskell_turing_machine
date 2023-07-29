module Prefabs.LRAnt(genAnt) where
import TuringMachine2D 
import State.State
import Tape.Tape2D
import State.Transition2D
import Debug.Trace

q0 :: State
q0 = State False "0"

dict :: [BColor]
dict = [ Black
        ,White 
        , Blue
        , Red
        , Green
        , Yellow
        , Cyan
        , Magenta
        , Orange
        , Pink
        , Purple
        , Brown
        , Grey
        , DarkGrey
        , LightGrey
        , DarkBlue
        , DarkRed
        , DarkGreen ]

data BColor =
          Black
        | White 
        | Blue
        | Red
        | Green
        | Yellow
        | Cyan
        | Magenta
        | Orange
        | Pink
        | Purple
        | Brown
        | Grey
        | DarkGrey
        | LightGrey
        | DarkBlue
        | DarkRed
        | DarkGreen 
          deriving (Eq)

instance Show BColor where
    -- now background 
    show White = "\ESC[0m█"
    show Black = "\ESC[30m█"
    show Blue = "\ESC[34m█"
    show Red = "\ESC[31m█"
    show Green = "\ESC[32m█"
    show Yellow = "\ESC[33m█"
    show Cyan = "\ESC[36m█"
    show Magenta = "\ESC[35m█"
    show Orange = "\ESC[91m█"
    show Pink = "\ESC[95m█"
    show Purple = "\ESC[94m█"
    show Brown = "\ESC[33m█"
    show Grey = "\ESC[90m█"
    show DarkGrey = "\ESC[90m█"
    show LightGrey = "\ESC[37m█"
    show DarkBlue = "\ESC[34m█"
    show DarkRed = "\ESC[31m█"
    show DarkGreen = "\ESC[32m█"
    

rotToColors :: [Rotation] -> [BColor]
rotToColors r = take (length r) dict

statesToTransitions :: Int -> [BColor] -> [Rotation] -> TransitionTable2D BColor
statesToTransitions _ _ [] = []
statesToTransitions _ [] _ = []
statesToTransitions i cs (r:rs) = transition : statesToTransitions (i+1) cs rs
        where transition =  Transition2D q0 q0 
                            (cs !! i) 
                            r
                            (cs !! ((i + 1) `mod` length cs)) 

genAnt :: [Rotation] -> TuringMachine2D BColor
genAnt r = beginTuring2 (beginTape2 (dict !! 0)) (statesToTransitions 0 (rotToColors r) r) q0