module Display(sizeX, sizeY, window) where
import Graphics.Gloss
sizeX :: Int
sizeX = 800

sizeY :: Int
sizeY = 600

window :: Display
window = InWindow "Turing Machine" (sizeX, sizeY) (10, 10)