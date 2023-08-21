module Tm.Prefabs.LRAnt(genAnt) where
import Tm.TuringMachine2D 
import Tm.State.State
import Tm.Tape.Tape2D

q0 :: State
q0 = State False "0"

datas :: [Int]
datas = [0..]

rotToColors :: [Rotation] -> [Int]
rotToColors r = take (length r) datas

stringToRotation :: String -> [Rotation]
stringToRotation [] = []
stringToRotation (x:xs) =
                if not (null xs) && head xs `elem` ['1'..'9'] then Numb (direct x) ((read [head xs]) :: Int) : stringToRotation (tail xs)
                else direct x : stringToRotation xs
        where direct c
                | c == 'L' || c == 'l' = RLeft
                | c == 'R' || c == 'r' = RRight
                | c == 'N' || c == 'n' = RNot
                | c == 'B' || c == 'b' = RBackward
                | otherwise = RNot

statesToTransitions :: Int -> [Int] -> [Rotation] -> TransitionTable Int Rotation
statesToTransitions _ _ [] = []
statesToTransitions _ [] _ = []
statesToTransitions i cs (r:rs) = transition : statesToTransitions (i+1) cs rs
        where transition =  mkTr q0 q0 
                            (cs !! i) 
                            r
                            (cs !! ((i + 1) `mod` length cs)) 

genAnt :: String -> TuringMachine2D Int
genAnt r = beginTuring2 tp trs q0
        where rot = stringToRotation r
              tp = beginTape2 (head datas)
              trs = statesToTransitions 0 (rotToColors rot) rot