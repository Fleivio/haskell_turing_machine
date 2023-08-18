module Main (main) where

import TuringMachine2D
import Prefabs.LRAnt
import Tape.Tape2D
import Img.Ppm

writeTmFile :: String -> IO()
writeTmFile actions = do
    let m = tmControlledRun2 999999 (genAnt actions)
        colorTable = m >>= \x -> Just $ getContent (tape x)
    maybe (print "Its Joever") (ppmWriteFile ("./images/"++actions++".ppm")) colorTable

main :: IO () 
main = do
    writeTmFile "LRRL"