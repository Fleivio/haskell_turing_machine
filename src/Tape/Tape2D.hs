module Tape.Tape2D(Tape2D(..), Rotation(..), getContent, beginTape2, beginTapeFromMat2, tapeRead2, tapeWrite2, tapeRotate2, beginTapeFromMatAndList2) where

import Tape.InfMatrix

data Direction2D = R| L| U | D deriving (Eq, Show)
data Rotation = RLeft | RRight | RNot | RBackward 
                | Numb Rotation Int deriving (Eq, Show)

rotateDir :: Direction2D -> Rotation -> Direction2D
--rotations
rotateDir R RLeft = U
rotateDir R RRight = D
rotateDir R RBackward = L
rotateDir L RLeft = D
rotateDir L RRight = U
rotateDir L RBackward = R
rotateDir U RLeft = L
rotateDir U RRight = R
rotateDir U RBackward = D
rotateDir D RLeft = R
rotateDir D RRight = L
rotateDir D RBackward = U
rotateDir d RNot = d

--multiple rotations
rotateDir d (Numb r n) 
    | n <= 0 = d
    | n == 1 = rotateDir d r
    | otherwise = rotateDir (rotateDir d r) (Numb r (n-1))

data Tape2D a = Tape2D {
                    pin :: Index2D,
                    dir :: Direction2D,
                    mat :: InfMatrix a
                } deriving Eq

instance Show a => Show (Tape2D a) where
    show (Tape2D p _ m) = showInfMatrix m ++ " " ++ show p

getContent :: Tape2D a -> [[a]]
getContent (Tape2D _ _ m) = map content (content mat)
    where mat = adjustMat m

beginTape2 :: a -> Tape2D a
beginTape2 b = Tape2D (0,0) U (mkInfMat b)

beginTapeFromMat2 :: [[a]] -> a -> Tape2D a
beginTapeFromMat2 l b = Tape2D (0,0) U (mkInfMatFromMat l b)

beginTapeFromMatAndList2 :: [[a]] -> a -> [a] -> Tape2D a
beginTapeFromMatAndList2 l b bs= Tape2D (0,0) U (mkInfMatFromMatAndList l b bs)

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