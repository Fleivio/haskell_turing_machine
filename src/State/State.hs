module State.State(State(..), Action(..), Transition, TransitionTable, mkTr, getTransition, nextAction) where

data State = State {
                isAccept :: Bool,
                name :: String
            } deriving (Eq)

data Action a b = Action {
                    nextState :: State,
                    charToWrite :: a,
                    dirToMove :: b
                }
                | Fail
                deriving Eq

data Transition a b = Transition {
                        from :: State,
                        tread :: a,
                        action :: Action a b
                    } deriving Eq

instance (Show a, Show b) => Show (Action a b) where 
    show a = name (nextState a) ++ "(" ++ show (charToWrite a) ++ ", " ++ show (dirToMove a) ++ ")"

instance (Show a, Show b) => Show (Transition a b) where 
    show a = name (from a) ++ "-" ++ show (tread a) ++ " -> " ++ show (action a) ++ ")"

type TransitionTable a b = [Transition a b] 

instance Show State where
    show a = name a ++ (if isAccept a then " (accept)" else "")

mkTr :: State -> State -> a -> b -> a -> Transition a b
mkTr fromState toState readChar dirToMove writeChar = Transition fromState readChar (Action toState writeChar dirToMove)

getTransition :: (Eq a) => a -> State -> TransitionTable a b -> [Transition a b]
getTransition readChar currentState transTable = [ tr | tr <- transTable, from tr == currentState, tread tr == readChar]

nextAction :: (Eq a) => a -> State -> TransitionTable a b -> [Action a b]
nextAction readChar currentState transTable = case getTransition readChar currentState transTable of
                                                [] -> [Fail]
                                                x -> map action x
                                            