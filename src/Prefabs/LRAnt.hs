module Prefabs.LRAnt(genAnt) where
import TuringMachine2D 
import State.State
import Tape.Tape2D
import State.Transition2D
import Debug.Trace
import Img.Color

q0 :: State
q0 = State False "0"

rotToColors :: [Rotation] -> [RGB]
rotToColors r = take (length r) dict

stringToRotation :: String -> [Rotation]
stringToRotation [] = []
stringToRotation (x:xs) =
                if not (null xs) && head xs `elem` ['1'..'9'] then Numb (dir x) ((read [head xs]) :: Int) : stringToRotation (tail xs)
                else dir x : stringToRotation xs
        where dir c
                | c == 'L' || c == 'l' = RLeft
                | c == 'R' || c == 'r' = RRight
                | c == 'N' || c == 'n' = RNot
                | c == 'B' || c == 'b' = RBackward
                | otherwise = RNot

statesToTransitions :: Int -> [RGB] -> [Rotation] -> TransitionTable2D RGB
statesToTransitions _ _ [] = []
statesToTransitions _ [] _ = []
statesToTransitions i cs (r:rs) = transition : statesToTransitions (i+1) cs rs
        where transition =  Transition2D q0 q0 
                            (cs !! i) 
                            r
                            (cs !! ((i + 1) `mod` length cs)) 

genAnt :: String -> TuringMachine2D RGB
genAnt r = beginTuring2 (beginTape2 (dict !! 0)) (statesToTransitions 0 (rotToColors (trace (show rot) rot)) rot) q0
        where rot = stringToRotation r