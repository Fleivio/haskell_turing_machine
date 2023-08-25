module Tm.Tape.Tape2D(Tape2D(..), getContent, mkTape2, mkTapeFromMat2, tapeRead2, tapeWrite2, tapeRotate2, mkTapeFromMatAndList2) where

import Tm.Tape.Basic.InfMatrix
import Tm.Tape.Basic.Rotation

data Tape2D a = Tape2D {
                    pin :: Index2D,
                    dir :: Direction2D,
                    mat :: InfMatrix a
                } deriving Eq

instance Show a => Show (Tape2D a) where
    show (Tape2D p _ m) = showInfMatrix m ++ " " ++ show p

getContent :: Tape2D a -> [[a]]
getContent (Tape2D _ _ m) = map content (content adMat)
    where adMat = adjustMat m

mkTape2 :: a -> Tape2D a
mkTape2 b = Tape2D (0,0) L (mkInfMat b)

mkTapeFromMat2 :: [[a]] -> a -> Tape2D a
mkTapeFromMat2 l b = Tape2D (0,0) U (mkInfMatFromMat l b)

mkTapeFromMatAndList2 :: [[a]] -> a -> [a] -> Tape2D a
mkTapeFromMatAndList2 l b bs= Tape2D (0,0) U (mkInfMatFromMatAndList l b bs)

tapeRead2 :: Tape2D a -> a
tapeRead2 t = mat t `mAcc` pin t  

tapeWrite2 :: Tape2D a -> a -> Tape2D a
tapeWrite2 t v = t{ mat = mSet (mat t) (pin t) v }

tapeRotate2 :: Tape2D a -> Rotation -> Tape2D a
tapeRotate2 v r = tapeStep (v { dir = rotateDir (dir v) r }) 

tapeStep :: Tape2D a -> Tape2D a
tapeStep t = t { pin = cDir (pin t) (dir t) } 
    where cDir (x,y) R = (x-1,y)
          cDir (x,y) L = (x+1,y)
          cDir (x,y) U = (x,y+1)
          cDir (x,y) D = (x,y-1)