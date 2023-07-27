module Main (main) where

import TuringMachine
import Tape.Tape
import State.Transition


tp1 :: Tape Integer
tp1 = beginTapeFromList [1,1,1,1] 0

s1 :: State
s1 = State False "s1"
s2 :: State
s2 = State False "s2"
s3 :: State
s3 = State True "s3"


t1 :: Transition Integer
t1 = Transition s1 s1 1 R 1
t2 :: Transition Integer
t2 = Transition s1 s2 0 L 2
t3 :: Transition Integer
t3 = Transition s2 s2 1 L 1
t4 :: Transition Integer
t4 = Transition s2 s3 0 R 2

ttest :: TuringMachine Integer
ttest =  TM tp1 [t1, t2, t3, t4] s1 False

main :: IO ()
main = do
        let v = tmRun ttest
        print v