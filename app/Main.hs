module Main (main) where

import TuringMachine2D
import Prefabs.LRAnt
import Tape.Tape2D

import Img.Color
import Img.Svg
import Img.Ppm

writeTmFile :: String -> IO()
writeTmFile actions = do
    let m = tmControlledRun2 200000 (genAnt actions)
        colorTable = m >>= \x -> Just $ map (map toRGB) (getContent (tape x))
    -- print m
    maybe (print "Its Joever") (svgWriteFile (actions++".svg")) colorTable
    maybe (print "Its Joever") (ppmWriteFile (actions++".ppm")) colorTable


main :: IO ()
main = do
    writeTmFile "RLLR"