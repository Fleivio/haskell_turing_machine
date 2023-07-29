module Main (main) where
import Prefabs.LangtonAnt
import TuringMachine2D
import Prefabs.LRAnt
import Tape.Tape2D

main :: IO ()
main = tmControlledRun2 200000 (genAnt [ RLeft, RRight, RRight, RRight, RRight, RRight, RLeft, RLeft, RRight ])