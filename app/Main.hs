module Main (main) where

import TuringMachine2D
import Prefabs.LRAnt
import Tape.Tape2D
import Img.Ppm
import Img.Color
import Img.Palette

palette = dracula

writeTmFile :: String -> IO()
writeTmFile actions = do
    let m = tmControlledRun2 9999999 (genAnt actions)
        table = m >>= \x -> Just $ getContent (tape x)
        colorTable = table >>= \x -> Just $ map (map (palette !!)) x 
    maybe (print "Its Joever") (ppmWriteFile ("./images/"++actions++".ppm")) colorTable

main :: IO () 
main = do
    writeTmFile "LRRL"