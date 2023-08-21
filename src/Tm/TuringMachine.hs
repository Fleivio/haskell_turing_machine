module Tm.TuringMachine(TuringMachine(..), tmRun, beginTuring, tmControlledRun) where
import Tm.Tape.Tape
import Tm.State.State

data TuringMachine a = TM {
        tape :: Tape a,
        transitionTable :: TransitionTable a Direction,
        currentState :: State,
        halt :: Bool,
        count :: Int
    }

beginTuring :: Tape a -> TransitionTable a Direction -> State -> TuringMachine a
beginTuring tp transTable state = TM tp transTable state False 0

instance (Show a) => Show (TuringMachine a) where
    show (TM _ _ st h c) = "\nSteps: " ++ show c
                               ++ "\nAccepted: " ++ show (h && isAccept st)

tmPerformAction :: TuringMachine a -> Action a Direction -> TuringMachine a
tmPerformAction tm Fail = tm { halt = True }
tmPerformAction tm@(TM t _ _ _ c) (Action nxt wChar dir)
                    = tm {
                        tape = tapeShift (tapeWrite t wChar) dir,
                        currentState = nxt,
                        count = c + 1}

tmStep :: (Eq a) => TuringMachine a -> [TuringMachine a]
tmStep tm@(TM _ _ _ True _) = pure tm
tmStep tm = map (tmPerformAction tm) actions
    where actions = nextAction (tapeRead (tape tm)) (currentState tm) (transitionTable tm)

tmRun' :: (Eq a, Show a) => [TuringMachine a] -> Maybe (TuringMachine a)
tmRun' tms
  | null tms = Nothing
  | not (null successful) = Just . head $ successful
  | otherwise = tmRun' continue
  where
      onNext = concatMap tmStep tms
      hadStopped = filter halt onNext
      continue = filter (not . halt) onNext
      successful = filter ( isAccept . currentState ) hadStopped

tmControlledRun' :: (Eq a, Show a) => Int -> [TuringMachine a] -> Maybe (TuringMachine a)
tmControlledRun' n tms
  | null tms = Nothing
  | n == 0   = Just (head tms)
  | n < 0    = Nothing
  | not (null successful) = Just . head $ successful
  | otherwise = tmControlledRun' (n - 1) continue
  where
      onNext = concatMap tmStep tms
      hadStopped = filter halt onNext
      continue = filter (not . halt) onNext
      successful = filter ( isAccept . currentState ) hadStopped

tmRun :: (Eq a, Show a) => TuringMachine a -> Maybe (TuringMachine a)
tmRun tm@(TM _ _ _ True _) = Just tm 
tmRun tm = result
    where result = tmRun' [tm]

tmControlledRun :: (Eq a, Show a) => Int -> TuringMachine a -> Maybe (TuringMachine a)
tmControlledRun _ tm@(TM _ _ _ True _) = Just tm
tmControlledRun n tm = result
    where result = tmControlledRun' n [tm]