module Runner(writeTmFile, runGloss) where

import Screen.Graphic
import Tm.TuringMachine2D
import Tm.Tape.ExpTape2
import Img.Ppm
import Img.Palette


writeTmFile :: TuringMachine2D Int -> Palette -> Int -> String -> IO()
writeTmFile tm pl maxSteps file = do
    let m = tmControlledRun2 maxSteps tm
        table = m >>= \x -> Just $ getContent (tape x)
        colorTable = table >>= \x -> Just $ map (map (pl !!)) x 
    maybe (print "Its Joever") (ppmWriteFile ("./images/"++file++".ppm")) colorTable

runGloss :: TuringMachine2D Int -> Palette -> Int -> IO()
runGloss = runTmGloss 