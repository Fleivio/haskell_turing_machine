module Img.Ppm (ppmWriteFile) where

import Img.Color

makePgm :: [[RGB]] -> String
makePgm xs = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify (concat xs)
                  where stringify [] = ""
                        stringify ((RGB r g b) : xs') = show (scale255 r) ++ " "
                                                        ++ show (scale255 g) ++ " "
                                                        ++ show (scale255 b) ++ " "
                                                        ++ stringify xs'
                        width = length (head xs) 
                        height = length xs 

ppmWriteFile :: String -> [[RGB]] -> IO()
ppmWriteFile path img = writeFile path (makePgm img)