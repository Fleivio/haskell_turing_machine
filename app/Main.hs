module Main (main) where

import Graphics.Gloss
import Graphic
import Tm.TuringMachine2D
import Prefabs.LRAnt
import qualified Img.Color as C
import Display (window)

background :: Color
background = makeColor 0 0 0 255

initial :: TuringMachine2D C.RGB
initial = tmStep (genAnt "RLNR2L")

main :: IO ()
main = play window -- tela
            background -- background padrão
            5000 -- fps
            initial
            tmToPic   -- print
            (const id)  -- input
            (const tmStep)  -- step