module Prefabs.LRAnt(genAnt) where
import Tm.TuringMachine2D 
import State.State
import Tape.Tape2D
import Debug.Trace
import Img.Color
import Img.Palette

q0 :: State
q0 = State False "0"

palette :: [RGB]
palette = dracula

rotToColors :: [Rotation] -> [RGB]
rotToColors r = take (length r) palette

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

statesToTransitions :: Int -> [RGB] -> [Rotation] -> TransitionTable RGB Rotation
statesToTransitions _ _ [] = []
statesToTransitions _ [] _ = []
statesToTransitions i cs (r:rs) = transition : statesToTransitions (i+1) cs rs
        where transition =  mkTr q0 q0 
                            (cs !! i) 
                            r
                            (cs !! ((i + 1) `mod` length cs)) 

genAnt :: String -> TuringMachine2D RGB
genAnt r = beginTuring2 tp trs q0
        where rot = stringToRotation r
              tp = beginTape2 (head palette)
              trs = statesToTransitions 0 (rotToColors (trace (show rot) rot)) rot