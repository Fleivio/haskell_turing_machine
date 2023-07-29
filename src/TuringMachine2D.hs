module TuringMachine2D(TuringMachine2D(..), tmRun2, beginTuring2, tmControlledRun2) where
import Tape.Tape2D
import State.Transition2D

import Img.Svg
import Data.Maybe (isNothing)

data TuringMachine2D a = TM2 {
        tape :: Tape2D a,
        transitionTable :: TransitionTable2D a,
        currentState :: State,
        halt :: Bool,
        count :: Int
    }

beginTuring2 :: Tape2D a -> TransitionTable2D a -> State -> TuringMachine2D a
beginTuring2 tape transTable state = TM2 tape transTable state False 0

instance (Show a) => Show (TuringMachine2D a) where
    show tm@(TM2 _ _ st h c) =    showTapeState tm
                               ++ "\nSteps: " ++ show c
                               ++ "\nAccepted: " ++ show (h && isAccept st)

showTapeState :: (Show a) => TuringMachine2D a -> String
showTapeState tm = show (tape tm) ++ " " ++ show (currentState tm)

tmPerformAction :: TuringMachine2D a -> Action2D a -> TuringMachine2D a
tmPerformAction tm Fail = tm { halt = True }
tmPerformAction tm@(TM2 t _ _ _ c) (Action2D nxt wChar dir)
                    = tm {
                        tape = tapeRotate2 (tapeWrite2 t wChar) dir,
                        currentState = nxt,
                        count = c + 1}

tmStep :: (Eq a) => TuringMachine2D a -> [TuringMachine2D a]
tmStep tm@(TM2 _ _ _ True _) = pure tm
tmStep tm = map (tmPerformAction tm) actions
    where actions = nextAction (tapeRead2 (tape tm)) (currentState tm) (transitionTable tm)

tmRun2' :: (Eq a, Show a) => [TuringMachine2D a] -> Maybe (TuringMachine2D a)
tmRun2' [] = Nothing
tmRun2' tms
  | not (null successful) = Just . head $ successful
  | otherwise = tmRun2' continue
  where
      onNext = concatMap tmStep tms
      hadStopped = filter halt onNext
      continue = filter (not . halt) onNext
      successful = filter (isAccept . currentState) hadStopped

tmControlledRun2' :: (Eq a, Show a) => Int -> [TuringMachine2D a] -> Maybe (TuringMachine2D a)
tmControlledRun2' _ [] = Nothing
tmControlledRun2' n tms
  | n == 0 = Just (head tms)
  | n < 0 = Nothing
  | not (null successful) = Just . head $ successful
  | otherwise = tmControlledRun2' (n - 1) continue
  where
      onNext = concatMap tmStep tms
      hadStopped = filter halt onNext
      continue = filter (not . halt) onNext
      successful = filter (isAccept . currentState) hadStopped

tmRun2 :: (Eq a, Show a) => TuringMachine2D a -> Maybe (TuringMachine2D a)
tmRun2 tm@(TM2 _ _ _ True _) = Just tm
tmRun2 tm = result
    where result = tmRun2' [tm]

tmControlledRun2 :: (Eq a, Show a) => Int -> TuringMachine2D a -> Maybe (TuringMachine2D a)
tmControlledRun2 _ tm@(TM2 _ _ _ True _) = Just tm 
tmControlledRun2 n tm = result
    where result = tmControlledRun2' n [tm]
