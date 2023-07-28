module Tape.InfMatrix(InfMatrix, Index2D, beginInfMat, mAcc, mSet, beginInfMatFromMat, beginInfMatFromMatAndList, showInfMatrix) where

import Tape.InfList

type Index2D = (Index, Index)
type InfMatrix a = InfList (InfList a)

showInfMatrix :: (Show a) => InfMatrix a -> String
showInfMatrix m = concatMap adjustShow (content m)
    where 
        smallestIndex = minimum $ map minIndex (content m)
        adjustShow :: (Show a) => InfList a -> String
        adjustShow row = show ( replicate (minIndex row - smallestIndex) (basic row) ++ content row) ++ "\n"

beginInfMat :: a -> InfMatrix a
beginInfMat a = beginInfList (beginInfList a)

beginInfMatFromMat :: [[a]] -> a -> InfMatrix a
beginInfMatFromMat ls b = beginInfListFromList subLists basicList
    where subLists = [beginInfListFromList l b | l <- ls]
          basicList = beginInfList b

beginInfMatFromMatAndList :: [[a]] -> a -> [a] -> InfMatrix a
beginInfMatFromMatAndList ls b bs = beginInfListFromList subLists basicList
    where subLists = [beginInfListFromList l b | l <- ls]
          basicList = beginInfListFromList bs b

mAcc :: InfMatrix a -> Index2D -> a
mAcc m (x,y) = m <!> y <!> x

mSet :: InfMatrix a -> Index2D -> a -> InfMatrix a
mSet m (x, y) v = lSet m y (lSet (m <!> y) x v)

