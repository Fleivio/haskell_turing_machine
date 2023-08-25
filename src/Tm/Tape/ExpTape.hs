module Tm.Tape.ExpTape(Tape(..), mkTape, tapeRead, tapeWrite, tapeShift, content) where
import Tm.Tape.Basic.Direction

data Tape a = Tape {
    left :: [a],
    right :: [a],
    base :: a
    } deriving Eq

instance Show a => Show (Tape a) where
    show t = show (reverse (left t)) ++ "y" ++ show (right t)

mkTape :: a -> Tape a
mkTape = Tape [] []

tapeRead :: Tape a -> a
tapeRead (Tape _ [] b) = b
tapeRead (Tape _ (x:_) _) = x

tapeWrite :: Tape a -> a -> Tape a
tapeWrite (Tape l [] b) v = Tape l [v] b
tapeWrite (Tape l (_:r) b) v = Tape l (v:r) b

tapeShift :: Tape a -> Direction -> Tape a
tapeShift (Tape [] rs b) L = Tape [] (b:rs) b
tapeShift (Tape ls [] b) R = Tape (b:ls) [] b
tapeShift (Tape (l:ls) rs b) L = Tape ls (l:rs) b
tapeShift (Tape ls (r:rs) b) R = Tape (r:ls) rs b

content :: Tape a -> [a]
content (Tape l r b) = [b] ++ reverse l ++ r ++ [b]

