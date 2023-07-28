module Tape.InfList( InfList(..), Index, beginInfList, beginInfListFromList, setVL, (<!>)) where 

type Index = Int
data InfList a = InfList { content :: [a], basic :: a, minIndex :: Index, maxIndex :: Index}
    deriving Eq

instance Show a => Show (InfList a) where
    show l = show (basic l) ++ show (content l) ++ show (basic l)

beginInfList :: a -> InfList a
beginInfList b = InfList [] b 0 (-1)

beginInfListFromList :: [a] -> a -> InfList a
beginInfListFromList l b = InfList l b 0 (length l - 1)

isOutOfBounds :: InfList a -> Index -> Bool
isOutOfBounds l i = i < minIndex l || i > maxIndex l

relativePosition :: InfList a -> Index -> Ordering
relativePosition l i
    | i < minIndex l = LT
    | i > maxIndex l = GT
    | otherwise = EQ

(<!>) :: InfList a -> Index -> a
(<!>) l i
    | isOutOfBounds l i = basic l
    | otherwise = content l !! (i - minIndex l)

setListValue :: [a] -> Index -> a -> [a]
setListValue [] _ _ = []
setListValue (_:l) 0 v = v : l
setListValue (x:l) i v = x : setListValue l (i-1) v

setVL :: InfList a -> Index -> a -> InfList a
setVL l i v = case relativePosition l i of
    LT -> l {content = newContentLeft, minIndex = i}
    GT -> l {content = newContentRight, maxIndex = i}
    EQ -> l {content = newContent}
    where newContent = setListValue (content l) (i - minIndex l) v
          newContentLeft = v : replicate (minIndex l - i - 1) (basic l) ++ content l
          newContentRight = content l ++ replicate (i - maxIndex l - 1) (basic l) ++ [v]

instance Functor InfList where
    fmap f n@(InfList l b _ _) = n { content = f <$> l, basic = f b}

instance Applicative InfList where
    pure = beginInfList
    (InfList l1 b1 _ _) <*> n@(InfList l2 b2 _ _) = n { content = l1 <*> l2, basic = b1 b2 }

instance Semigroup a => Semigroup (InfList a) where
    (InfList l1 b1 _ _) <> (InfList l2 b2 _ _) = InfList (zipWith (<>) l1 l2) (b1 <> b2) 0 (min (length l1) (length l2) - 1)

instance Monoid a => Monoid (InfList a) where
    mempty = beginInfList mempty

_lTest :: InfList Int
_lTest = setVL (setVL (beginInfList 0) (-2) 9) 10 8

_lTest2 :: InfList Integer
_lTest2 = beginInfListFromList [1..] (-15)
