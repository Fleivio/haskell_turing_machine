module Main (main) where

import Tm.TuringMachine2D
import Tm.Prefabs.LRAnt
import Img.Palette

import Runner

import Screen.Graphic

config :: String
config = "LRRL"

palette :: Palette
palette = spring

machine :: TuringMachine2D Int 
machine = tmStep $ genAnt config

-- main :: IO () 
-- main = do
--     writeTmFile machine palette 9000 config

main :: IO ()
main = runGloss machine palette 10000

