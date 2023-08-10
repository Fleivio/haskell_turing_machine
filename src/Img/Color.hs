{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Img.Color (RGB (..), Color (..), black, white, dict, scale255, from255) where

class Color a where
  colAdd :: a -> a -> a
  colMult :: a -> Double -> a
  colCombine :: a -> a -> a
  clamp :: a -> a
  toGray :: a -> Double

data RGB = RGB {rColor :: Double, gColor :: Double, bColor :: Double} deriving (Eq)

instance Show RGB where
  show (RGB r g b) = "(" ++ show r ++ " " ++ show g ++ " " ++ show b ++ ")"

instance Color RGB where
  colAdd (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)
  colMult (RGB r g b) d = RGB (r * d) (g * d) (b * d)
  colCombine (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1 * r2) (g1 * g2) (b1 * b2)
  clamp (RGB r1 g1 b1) = RGB (clamp' r1) (clamp' g1) (clamp' b1)
    where
      clamp' x = max 0 (min x 1)
  toGray (RGB r1 g1 b1) = 0.299 * r1 + 0.587 * g1 + 0.114 * b1

from255 :: (Int, Int, Int) -> RGB
from255 (r, g, b) = RGB (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255)

scale255 :: Double -> Int
scale255 x = round (x * 255)

black :: RGB
black = RGB 0 0 0

white :: RGB
white = RGB 1 1 1

red :: RGB
red = RGB 1 0 0

blue :: RGB
blue = RGB 0 0 1

green :: RGB
green = RGB 0 1 0

yellow :: RGB
yellow = RGB 1 1 0

cyan :: RGB
cyan = RGB 0 1 1

magenta :: RGB
magenta = RGB 1 0 1

brown :: RGB
brown = RGB 0.5 0.25 0

orange :: RGB
orange = RGB 1 0.5 0

pink :: RGB
pink = RGB 1 0.75 0.8

purple :: RGB
purple = RGB 0.5 0 0.5

gray :: RGB
gray = RGB 0.5 0.5 0.5

lightGray :: RGB
lightGray = RGB 0.75 0.75 0.75

darkGray :: RGB
darkGray = RGB 0.25 0.25 0.25

darkRed :: RGB
darkRed = RGB 0.5 0 0

darkGreen :: RGB
darkGreen = RGB 0 0.5 0

darkBlue :: RGB
darkBlue = RGB 0 0 0.5

darkYellow :: RGB
darkYellow = RGB 0.5 0.5 0

darkCyan :: RGB
darkCyan = RGB 0 0.5 0.5

darkMagenta :: RGB
darkMagenta = RGB 0.5 0 0.5

darkBrown :: RGB
darkBrown = RGB 0.25 0.125 0

darkOrange :: RGB
darkOrange = RGB 0.5 0.25 0

darkPink :: RGB
darkPink = RGB 0.5 0.375 0.4

darkPurple :: RGB
darkPurple = RGB 0.25 0 0.25

lightRed :: RGB
lightRed = RGB 1 0.5 0.5

lightGreen :: RGB
lightGreen = RGB 0.5 1 0.5

lightBlue :: RGB
lightBlue = RGB 0.5 0.5 1

lightYellow :: RGB
lightYellow = RGB 1 1 0.5

dict :: [RGB]
dict =
  [ black,
    white,
    purple,
    orange,
    blue,
    red,
    green,
    yellow,
    cyan,
    magenta,
    brown,
    pink,
    gray,
    lightGray,
    darkGray,
    darkRed,
    darkGreen,
    darkBlue,
    darkYellow,
    darkCyan,
    darkMagenta,
    darkBrown,
    darkOrange,
    darkPink,
    darkPurple,
    lightRed,
    lightGreen,
    lightBlue,
    lightYellow
  ]
