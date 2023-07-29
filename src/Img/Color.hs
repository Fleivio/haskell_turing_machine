{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Img.Color (RGB (..), Color (..), black, white, dict, scale255) where

class Color a where
  colMax :: Double
  colMin :: Double
  colAdd :: a -> a -> a
  colMult :: a -> Double -> a
  colCombine :: a -> a -> a
  clamp :: a -> a
  toGray :: a -> Double
  toASCII :: a -> Char

data RGB = RGB {rColor :: Double, gColor :: Double, bColor :: Double} deriving (Eq)

aciiTable :: String
aciiTable = "`.-':_,^=;><+!rc*/z?sLTv)J7(|Fi{C}fI31tlu[neoZ5Yxjya]2ESwqkP6h9d4VpOGbUAKXHm8RD#$Bg0MNWQ%&@"

brighLevels :: [Double]
brighLevels = [0, 0.0751, 0.0829, 0.0848, 0.1227, 0.1403, 0.1559, 0.185, 0.2183, 0.2417, 0.2571, 0.2852, 0.2902, 0.2919, 0.3099, 0.3192, 0.3232, 0.3294, 0.3384, 0.3609, 0.3619, 0.3667, 0.3737, 0.3747, 0.3838, 0.3921, 0.396, 0.3984, 0.3993, 0.4075, 0.4091, 0.4101, 0.42, 0.423, 0.4247, 0.4274, 0.4293, 0.4328, 0.4382, 0.4385, 0.442, 0.4473, 0.4477, 0.4503, 0.4562, 0.458, 0.461, 0.4638, 0.4667, 0.4686, 0.4693, 0.4703, 0.4833, 0.4881, 0.4944, 0.4953, 0.4992, 0.5509, 0.5567, 0.5569, 0.5591, 0.5602, 0.5602, 0.565, 0.5776, 0.5777, 0.5818, 0.587, 0.5972, 0.5999, 0.6043, 0.6049, 0.6093, 0.6099, 0.6465, 0.6561, 0.6595, 0.6631, 0.6714, 0.6759, 0.6809, 0.6816, 0.6925, 0.7039, 0.7086, 0.7235, 0.7302, 0.7332, 0.7602, 0.7834, 0.8037, 0.9999]

instance Show RGB where
  show (RGB r g b) = "(" ++ show r ++ " " ++ show g ++ " " ++ show b ++ ")"

instance Color RGB where
  colMax = 1
  colMin = 0
  colAdd (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)
  colMult (RGB r g b) d = RGB (r * d) (g * d) (b * d)
  colCombine (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1 * r2) (g1 * g2) (b1 * b2)
  clamp (RGB r1 g1 b1) = RGB (clamp' r1) (clamp' g1) (clamp' b1)
    where
      clamp' x = max 0 (min x 1)
  toGray (RGB r1 g1 b1) = 0.299 * r1 + 0.587 * g1 + 0.114 * b1

  toASCII c = aciiTable !! indx
    where
      indx = min (length aciiTable - 1) (round (toGray c * 100))

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
    red,
    blue,
    green,
    yellow,
    cyan,
    magenta,
    brown,
    orange,
    pink,
    purple,
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
