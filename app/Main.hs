module Main (main) where

import TuringMachine
import Prefabs.EvenPalindrome


main :: IO ()
main = do
        let v = tmRun tEvenPalindrome
        print v
