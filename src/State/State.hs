module State.State(State(..)) where

data State = State {
                isAccept :: Bool,
                name :: String
            } deriving (Eq)

instance Show State where
    show a = name a ++ (if isAccept a then " (accept)" else "")