module Tape.InfList(InfList(..), Index, mkInfList, mkInfListFromList, (<!>), lAcc, lSet, smallestIndex, largestIndex, content ) where 

type Index = Int

data InfList a = InfList {
    negative :: [a],
    positive :: [a],
    base :: a
    } deriving Eq

instance (Show a) => Show (InfList a) where
    show l = show (base l) ++ show (reverse (negative l)) ++ show (positive l) ++ show (base l)

instance Functor InfList where
    fmap f l = l {negative = f <$> negative l, positive = f <$> positive l, base = f (base l)} 

mkInfList :: a -> InfList a
mkInfList = InfList [] []

mkInfListFromList :: [a] -> a -> InfList a
mkInfListFromList = InfList []

relativePosition :: InfList a -> Index -> Ordering
relativePosition l i
    | i >= 0 && i >= length (positive l) = GT 
    | i < 0 && (-(i+1)) >= length (negative l) = LT
    | otherwise = EQ

isOutOfBounds :: InfList a -> Index -> Bool
isOutOfBounds l i = relativePosition l i /= EQ

(<!>) :: InfList a -> Index -> a
(<!>) l i
    | isOutOfBounds l i = base l
    | i < 0 = negative l !! (-(i + 1))
    | otherwise = positive l !! i

lAcc :: InfList a -> Index -> a
lAcc = (<!>)

setListValue :: [a] -> Index -> a -> [a]
setListValue [] _ _ = []
setListValue (_:l) 0 v = v : l
setListValue (x:l) i v = x : setListValue l (i-1) v

lSet :: InfList a -> Index -> a -> InfList a
lSet l i v
    | relativePosition l i == LT = l {negative = newContentLeft}
    | relativePosition l i == GT = l {positive = newContentRight}
    | otherwise = if i < 0 then l {negative = newContentL1} else l {positive = newContentL2}
    where 
        newContentLeft = negative l ++ replicate (index - length (negative l)) (base l) ++ [v]
        newContentRight = positive l ++ replicate (index - length (positive l)) (base l) ++ [v]
        newContentL1 = setListValue (negative l) index v 
        newContentL2 = setListValue (positive l) index v
        index = if i < 0 then -(i + 1) else i

smallestIndex :: InfList a -> Index
smallestIndex l = -(length (negative l) + 1)

largestIndex :: InfList a -> Index
largestIndex l = length (positive l)

content :: InfList a -> [a]
content l = reverse (negative l) ++ positive l