module Main (main) where

import TuringMachine2D
import Prefabs.LRAnt
import Tape.Tape2D
import Img.Svg
import Img.Ppm

writeTmFile :: String -> IO()
writeTmFile actions = do
    let m = tmControlledRun2 200000 (genAnt actions)
        colorTable = m >>= \x -> Just $ getContent (tape x)
    maybe (print "Its Joever") (svgWriteFile ("./images/"++actions++".svg")) colorTable
    maybe (print "Its Joever") (ppmWriteFile ("./images/"++actions++".ppm")) colorTable

main :: IO ()
main = do
    writeTmFile "LLRL"
    writeTmFile "RRLLLRLLLRRR"
    writeTmFile "LRRRRRLLR"
    writeTmFile "LLRR"
    writeTmFile "RLR"
    writeTmFile "LLLRL"
    writeTmFile "RRRLL"