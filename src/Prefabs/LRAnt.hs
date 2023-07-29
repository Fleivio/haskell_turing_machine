module Prefabs.LRAnt(genAnt) where
import TuringMachine2D 
import State.State
import Tape.Tape2D
import State.Transition2D
import Debug.Trace
import Img.Color

q0 :: State
q0 = State False "0"

rotToColors :: [Rotation] -> [DColor]
rotToColors r = take (length r) dict

stringToRotation :: String -> [Rotation]
stringToRotation [] = []
stringToRotation (x:xs)
        | x == 'L' || x == 'l' = RLeft : stringToRotation xs
        | x == 'R' || x == 'r' = RRight : stringToRotation xs
        | otherwise = stringToRotation xs

statesToTransitions :: Int -> [DColor] -> [Rotation] -> TransitionTable2D DColor
statesToTransitions _ _ [] = []
statesToTransitions _ [] _ = []
statesToTransitions i cs (r:rs) = transition : statesToTransitions (i+1) cs rs
        where transition =  Transition2D q0 q0 
                            (cs !! i) 
                            r
                            (cs !! ((i + 1) `mod` length cs)) 

genAnt :: String -> TuringMachine2D DColor
genAnt r = beginTuring2 (beginTape2 (dict !! 0)) (statesToTransitions 0 (rotToColors rot) rot) q0
        where rot = stringToRotation r