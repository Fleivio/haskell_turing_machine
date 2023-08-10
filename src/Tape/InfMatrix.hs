module Tape.InfMatrix(Index2D, InfMatrix, InfList(..), mkInfMat, mkInfMatFromMat, adjustMat, content, mkInfMatFromMatAndList, mAcc, mSet, showInfMatrix) where

import Tape.InfList

type Index2D = (Index, Index)
type InfMatrix a = InfList (InfList a)

showInfMatrix :: (Show a) => InfMatrix a -> String
showInfMatrix m = unlines $ map show (content (adjustMat m))

adjustMat :: InfMatrix a -> InfMatrix a
adjustMat m = m { positive = map adjustRow (positive m),
                  negative = map adjustRow (negative m) }
    where 
        smallestIdx = minimum $ map smallestIndex (content m)
        largestIdx = maximum $ map largestIndex (content m)

        adjustRow :: InfList a -> InfList a
        adjustRow row = mkInfListFromList
                        (replicate (smallestIndex row - smallestIdx) (base row)
                         ++ content row ++
                         replicate (largestIdx - largestIndex row) (base row)) (base row)


mkInfMat :: a -> InfMatrix a
mkInfMat a = mkInfList (mkInfList a)

mkInfMatFromMat :: [[a]] -> a -> InfMatrix a
mkInfMatFromMat ls b = mkInfListFromList subLists basicList
    where subLists = [mkInfListFromList l b | l <- ls]
          basicList = mkInfList b

mkInfMatFromMatAndList :: [[a]] -> a -> [a] -> InfMatrix a
mkInfMatFromMatAndList ls b bs = mkInfListFromList subLists basicList
    where subLists = [mkInfListFromList l b | l <- ls]
          basicList = mkInfListFromList bs b

mAcc :: InfMatrix a -> Index2D -> a
mAcc m (x,y) = m <!> y <!> x

mSet :: InfMatrix a -> Index2D -> a -> InfMatrix a
mSet m (x, y) v = lSet m y (lSet (m <!> y) x v)