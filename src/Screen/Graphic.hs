module Screen.Graphic(runTmGloss) where

import Tm.TuringMachine2D
import qualified Img.Color as Cl
import Img.Palette
import Tm.Tape.Tape2D
import Screen.Display

import qualified Data.ByteString as BS
import qualified Data.Word as D (Word8)

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss 

rgbToW8 :: Cl.RGB -> [D.Word8]
rgbToW8 c = map fromIntegral [Cl.scale255 (Cl.rColor c), Cl.scale255 (Cl.gColor c), Cl.scale255 (Cl.bColor c), 255]

convertColors :: [[Cl.RGB]] -> [D.Word8]
convertColors c = concatMap rgbToW8 c1
    where c1 = concat c

tmToPic :: Palette -> TuringMachine2D Int -> Picture
tmToPic pl tm = bitmapOfByteString
            (length (head colors))
            (length colors)
            (BitmapFormat TopToBottom PxRGBA)
            ((BS.pack . convertColors) colors)
            False
    where colors = map (map (pl !!)) values 
          values = getContent (tape tm)

background :: Color
background = makeColor 0 0 0 255

runTmGloss :: TuringMachine2D Int -> Palette -> Int -> IO ()
runTmGloss initialMachine colorPalette fps = play window -- tela
                    background -- background padrão
                    fps 
                    initialMachine
                    (tmToPic colorPalette)   -- print
                    (const id)  -- input
                    (const tmStep)  -- step
