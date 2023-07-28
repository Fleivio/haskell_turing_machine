module Main (main) where

import TuringMachine
import Prefabs.BB4 (tBB4)
import Prefabs.EvenPalindrome (tEvenPalindrome)
import Prefabs.BB5 (tBB5)

main :: IO ()
main = tmRun tEvenPalindrome