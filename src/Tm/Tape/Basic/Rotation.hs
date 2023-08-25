module Tm.Tape.Basic.Rotation(Rotation(..), Direction2D(..), rotateDir) where

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