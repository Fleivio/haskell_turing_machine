{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Img.Svg(svgGen, svgWriteFile) where

import Img.Color

defWidth :: Int
defWidth = 5

genStyle :: RGB -> String
genStyle (RGB r g b)= "fill:rgb(" ++ show r1 ++ "," ++ show g1 ++ "," ++ show b1 ++ ");stroke-width:1;stroke:rgb(0,0,0)"
    where (r1, g1, b1) = (scale255 r,  scale255 g, scale255 b)

svgRect :: RGB -> Int -> Int -> String
svgRect c x y = "<rect x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\" width=\""
                ++ show defWidth ++ "\" height=\"" ++ show defWidth ++ "\" style=\"" ++ genStyle c ++ "\"/>"

svgGenContent :: [[RGB]] -> String
svgGenContent [] = ""
svgGenContent xs = run xs 0
    where run (row : nextRow) y = svgGenRow row y 0 ++ run nextRow (y + defWidth)
          run [] _ = ""
          svgGenRow [] _ _ = ""
          svgGenRow (c : nextC) y x = svgRect c x y ++ svgGenRow nextC y (x + defWidth)

svgGen :: (Int, Int) -> String -> String
svgGen (x,y) content = "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"" ++ show x ++ "\" height=\"" ++ show y ++ "\">" ++ content ++ "</svg>"

svgWriteFile :: String -> [[RGB]] -> IO ()
svgWriteFile path img = writeFile path svgString
    where imgSize = (defWidth * length (head img), defWidth * length img)
          svgString = svgGen imgSize svgContent
          svgContent = svgGenContent img