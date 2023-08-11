module Graphic(tmToPic) where

import Tm.TuringMachine2D
import Img.Color
import Tape.Tape2D

import qualified Data.ByteString as BS
import qualified Data.Word as D (Word8)

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss
import Display

rgbToW8 :: RGB -> [D.Word8]
rgbToW8 c = map fromIntegral [scale255 (rColor c), scale255 (gColor c), scale255 (bColor c), 255]

convertColors :: [[RGB]] -> [D.Word8]
convertColors c = concatMap rgbToW8 c1
    where c1 = concat c

tmToPic :: TuringMachine2D RGB -> Picture
tmToPic tm = bitmapOfByteString
            (length (head colors))
            (length colors)
            (BitmapFormat TopToBottom PxRGBA)
            ((BS.pack . convertColors) colors)
            False
    where colors = (getContent . tape) tm