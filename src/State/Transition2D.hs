{-# OPTIONS_GHC -Wno-partial-fields #-}
module State.Transition2D(Transition2D(..), State(..), Action2D(..), TransitionTable2D, nextAction) where
import Tape.Tape2D
import State.State

data Transition2D a = Transition2D {
                    from :: State,
                    to :: State,
                    tread :: a,
                    direction :: Rotation,
                    twrite :: a
                } deriving Eq

instance (Show a) => Show (Transition2D a) where 
    show a = name (from a) ++ "-" ++ show (tread a) ++ " -> " ++ name (to a) ++ "(" ++ show (twrite a) ++ ", " ++ show (direction a) ++ ")"

data Action2D a = Action2D {
                    nextState :: State,
                    charToWrite :: a,
                    dirToMove :: Rotation
                } | Fail

type TransitionTable2D a = [Transition2D a] 

getTransition :: (Eq a) => a -> State -> TransitionTable2D a -> [Transition2D a]
getTransition readChar currentState transTable = [ tr | tr <- transTable, from tr == currentState, tread tr == readChar]

transitionToAction :: Transition2D a -> Action2D a
transitionToAction t = Action2D ( to t ) ( twrite t ) (direction t)

nextAction :: (Eq a) => a -> State -> TransitionTable2D a -> [Action2D a]
nextAction readChar currentState transTable = case getTransition readChar currentState transTable of
                                                [] -> [Fail]
                                                x -> map transitionToAction x
                                            