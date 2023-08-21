module Main (main) where

import TuringMachine2D
import Prefabs.LRAnt
import Tape.Tape2D
import Img.Ppm
import Img.Palette

palette :: Palette
palette = spring

writeTmFile :: String -> IO()
writeTmFile actions = do
    let m = tmControlledRun2 99999 (genAnt actions)
        table = m >>= \x -> Just $ getContent (tape x)
        colorTable = table >>= \x -> Just $ map (map (palette !!)) x 
    maybe (print "Its Joever") (ppmWriteFile ("./images/"++actions++".ppm")) colorTable

main :: IO () 
main = do
    writeTmFile "LRRL"