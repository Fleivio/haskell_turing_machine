{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Tm.Tape.ExpTape2(Tape2D(..), mkTape2, tapeRead2, tapeWrite2, tapeRotate2, getContent) where
-- import Tm.Tape.Basic.Rotation

import Tm.Tape.Basic.Rotation

data Tape2D a = Tape2D {
    topRight :: [[a]],
    topLeft :: [[a]],
    botLeft :: [[a]],
    botRight :: [[a]],
    direction :: Direction2D,
    base :: a
    } deriving Eq

instance Show a => Show (Tape2D a) where
    show tp = show (topLeft tp) ++ show (topRight tp) ++ "\n" ++ show (botLeft tp) ++ show (botRight tp)

mkTape2 :: a -> Tape2D a
mkTape2 = Tape2D [] [] [] [] R

swapLine2 :: a -> [[a]] -> [[a]] -> ([[a]], [[a]])
swapLine2 bs [] x = ([[bs]], [bs]:x)
swapLine2 _ (x:xs) ys = (xs, x:ys)

swapColumn :: a -> [[a]] -> [[a]] -> ([[a]], [[a]])
swapColumn bs f t = (fRest, prepColumn bs fCol t)
    where 
            (fCol, fRest) = popColumn bs f

popColumn :: a -> [[a]] -> ([a], [[a]])
popColumn bs [] = ([bs], [[bs]])
popColumn bs xs = (map sfHead xs, map sfTail xs)
    where 
            sfHead x = if null x then bs else head x 
            sfTail x = if null (tail x) then [bs] else tail x

prepColumn :: a -> [a] -> [[a]] -> [[a]]
prepColumn bs mat col =
    let mat' = mat ++ replicate (length col - length mat) bs
        col' = col ++ replicate (length mat - length col) [bs]
    in  zipWith (:) mat' col'


t1 = tapeWrite2 (Tape2D [] [] [] [] L 0) 1
t2 = tapeStep t1
t3 = tapeWrite2 t2 2

tapeRead2 :: Tape2D a -> a
tapeRead2 (Tape2D [] _ _ _ _ b) = b
tapeRead2 (Tape2D ([]:_) _ _ _ _ b) = b
tapeRead2 (Tape2D (x:_) _ _ _ _ _) = head x

tapeWrite2 :: Tape2D a -> a -> Tape2D a
tapeWrite2 (Tape2D [] l2 l3 l4 r b) v = Tape2D [[v]] l2 l3 l4 r b
tapeWrite2 (Tape2D ([]:lb) l2 l3 l4 r b) v = Tape2D ([v] : lb) l2 l3 l4 r b
tapeWrite2 (Tape2D ((_:la):lb) l2 l3 l4 r b) v = Tape2D ((v : la) : lb) l2 l3 l4 r b

tapeRotate2 :: Tape2D a -> Rotation -> Tape2D a
tapeRotate2 t r = tapeStep (t { direction = rotateDir (direction t) r })

tapeStep :: Tape2D a -> Tape2D a
tapeStep (Tape2D tr tl bl br d bs) = case d of
    U ->
        Tape2D utr utl ubl ubr d bs
        where
                (utr, ubr) = swapLine2 bs tr br
                (utl, ubl) = swapLine2 bs tl bl
    D ->
        Tape2D dtr dtl dbl dbr d bs
        where
                (dbr, dtr) = swapLine2 bs br tr
                (dbl, dtl) = swapLine2 bs bl tl
    R ->
        Tape2D rtr rtl rbl rbr d bs
        where
                (rtr, rtl) = swapColumn bs tr tl
                (rbr, rbl) = swapColumn bs br bl
    L ->
        Tape2D ltr ltl lbl lbr d bs
        where
                (ltl, ltr) = swapColumn bs tl tr
                (lbl, lbr) = swapColumn bs bl br


adjustRows :: a -> [[a]] -> [[a]] -> ([[a]], [[a]])
adjustRows bs a b
    | length a > length b = (a, b ++ replicate (length a - length b) [bs])
    | otherwise           = (a ++ replicate (length b - length a) [bs], b)

adjustColumns :: a -> [[a]] -> [[a]] -> ([[a]], [[a]])
adjustColumns bs a b = (completeCol a, completeCol b)
    where largestI = maximum [length ms | ms <- a ++ b]
          completeCol x = zipWith (++) x [replicate (largestI - length ms) bs | ms <- x]

adjustTape :: Tape2D a -> Tape2D a
adjustTape (Tape2D tr tl bl br d bs) = Tape2D tr2 tl2 bl2 br2 d bs
    where
            (tr1, tl1) = adjustRows bs tr tl
            (bl1, br1) = adjustRows bs bl br
            (tr2, br2) = adjustColumns bs tr1 br1
            (tl2, bl2) = adjustColumns bs tl1 bl1


getContent :: Tape2D a -> [[a]]
getContent tp =
    let (Tape2D tr tl bl br _ _) = adjustTape tp
        trRev = tr
        tlRev = map reverse tl
        brRev = reverse br
        blRev = map reverse (reverse bl)

        tlPlusTr = zipWith (++) tlRev trRev
        blPlusBr = zipWith (++) blRev brRev
    in blPlusBr ++ tlPlusTr 