module Tm.Tape.Tape (Tape(..), mkTape, mkTapeFromList, tapeRead, tapeWrite, tapeShift) where

import Tm.Tape.Basic.InfList
import Tm.Tape.Basic.Direction

data Tape a = Tape {
                    pin :: Index,
                    list :: InfList a
                } deriving Eq

instance Show a => Show (Tape a) where
    show t = show (list t) ++ " " ++ show (pin t)

mkTape :: a -> Tape a
mkTape b = Tape 0 (mkInfList b)

mkTapeFromList :: [a] -> a -> Tape a
mkTapeFromList l b = Tape 0 (mkInfListFromList l b)

tapeRead :: Tape a -> a
tapeRead t = list t <!> pin t  

tapeWrite :: Tape a -> a -> Tape a
tapeWrite t v = t{ list = lSet (list t) (pin t) v }

tapeShift :: Tape a -> Direction -> Tape a
tapeShift t L = t { pin = pin t - 1 }
tapeShift t R = t { pin = pin t + 1 }