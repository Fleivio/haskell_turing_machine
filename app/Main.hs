module Main (main) where
import Prefabs.LangtonAnt
import TuringMachine2D 
import TuringMachine
import Prefabs.BB5 (tBB5)


main :: IO ()
main = tmControlledRun 1200 tBB5