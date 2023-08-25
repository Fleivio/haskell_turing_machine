module Tm.Tape.Tape (Direction(..), Tape(..), beginTape, beginTapeFromList, tapeRead, tapeWrite, tapeShift) where

import Tm.Tape.Basic.InfList

data Direction = R | L deriving (Eq, Show)

data Tape a = Tape {
                    pin :: Index,
                    list :: InfList a
                } deriving Eq

instance Show a => Show (Tape a) where
    show t = show (list t) ++ " " ++ show (pin t)

beginTape :: a -> Tape a
beginTape b = Tape 0 (mkInfList b)

beginTapeFromList :: [a] -> a -> Tape a
beginTapeFromList l b = Tape 0 (mkInfListFromList l b)

tapeRead :: Tape a -> a
tapeRead t = list t <!> pin t  

tapeWrite :: Tape a -> a -> Tape a
tapeWrite t v = t{ list = lSet (list t) (pin t) v }

tapeShift :: Tape a -> Direction -> Tape a
tapeShift t L = t { pin = pin t - 1 }
tapeShift t R = t { pin = pin t + 1 }