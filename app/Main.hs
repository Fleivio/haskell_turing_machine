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
--     writeTmFile machine palette 90 config

main :: IO ()
main = runGloss machine palette 99999