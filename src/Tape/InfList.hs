module Tape.InfList( NList(..), Index, beginNList, beginNListFromList, setVL, (<!>)) where 

type Index = Int
data NList a = NList { content :: [a], basic :: a, maxIndex :: Index, minIndex :: Index }
    deriving Eq

instance Show a => Show (NList a) where
    show l = show (basic l) ++ show (content l) ++ show (basic l)

beginNList :: a -> NList a
beginNList b = NList [] b 0 0

beginNListFromList :: [a] -> a -> NList a
beginNListFromList l b = NList l b (length l - 1) 0

(<!>) :: NList a -> Index -> a
(<!>) l i
    | i < minIndex l || i > maxIndex l = basic l
    | otherwise = content l !! (i - minIndex l)

setListValue :: [a] -> Index -> a -> [a]
setListValue [] _ _ = []
setListValue (_:l) 0 v = v : l
setListValue (x:l) i v = x : setListValue l (i-1) v

setVL :: NList a -> Index -> a -> NList a
setVL l i v
    | i < minIndex l = l {content = newContentLeft, minIndex = i}
    | i > maxIndex l = l {content = newContentRight, maxIndex = i}
    | otherwise = l {content = newContent}
    where newContent = setListValue (content l) (i - minIndex l) v
          newContentLeft = v : replicate (minIndex l - i - 1) (basic l) ++ content l
          newContentRight = content l ++ replicate (i - maxIndex l - 1) (basic l) ++ [v]

instance Functor NList where
    fmap f n@(NList l b _ _) = n { content = f <$> l, basic = f b}

instance Applicative NList where
    pure = beginNList
    (NList l1 b1 _ _) <*> n@(NList l2 b2 _ _) = n { content = l1 <*> l2, basic = b1 b2 }

instance Monad NList where
    (NList _ b _ _) >>= f = f b


_lTest :: NList Int
_lTest = setVL (setVL (beginNList 0) (-2) 9) 10 8

_lTest2 :: NList Integer
_lTest2 = beginNListFromList [1..] (-15)
