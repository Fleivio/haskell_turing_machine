module Main (main) where

import TuringMachine
import Prefabs.BB4 (tBB4)
import Prefabs.EvenPalindrome (tEvenPalindrome)

main :: IO ()
main = tmRun tBB4
