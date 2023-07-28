{-# OPTIONS_GHC -Wno-partial-fields #-}
module State.Transition(Transition(..), State(..), Action(..), TransitionTable, nextAction) where
import Tape.Tape

data State = State {
                isAccept :: Bool,
                name :: String
            } deriving (Eq)

instance Show State where
    show a = name a ++ (if isAccept a then " (accept)" else "")

data Transition a = Transition {
                    from :: State,
                    to :: State,
                    tread :: a,
                    direction :: Direction,
                    twrite :: a
                } deriving Eq

instance (Show a) => Show (Transition a) where 
    show a = name (from a) ++ "-" ++ show (tread a) ++ " -> " ++ name (to a) ++ "(" ++ show (twrite a) ++ ", " ++ show (direction a) ++ ")"

data Action a = Action {
                    nextState :: State,
                    charToWrite :: a,
                    dirToMove :: Direction
                } | Fail

type TransitionTable a = [Transition a] 

getTransition :: (Eq a) => a -> State -> TransitionTable a -> [Transition a]
getTransition readChar currentState transTable = [ tr | tr <- transTable, from tr == currentState, tread tr == readChar]

transitionToAction :: Transition a -> Action a
transitionToAction t = Action ( to t ) ( twrite t ) (direction t)

nextAction :: (Eq a) => a -> State -> TransitionTable a -> [Action a]
nextAction readChar currentState transTable = case getTransition readChar currentState transTable of
                                                [] -> [Fail]
                                                x -> map transitionToAction x
                                            