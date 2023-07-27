module Main (main) where

import TuringMachine
import Prefabs.BB4 (tBB4)


main :: IO ()
main = do
        let v = tmRun tBB4
        print v
        print $ count v
