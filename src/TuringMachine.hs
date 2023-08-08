module TuringMachine(TuringMachine(..), tmRun, beginTuring, tmControlledRun) where
import Tape.Tape
import State.State

data TuringMachine a = TM {
        tape :: Tape a,
        transitionTable :: TransitionTable a Direction,
        currentState :: State,
        halt :: Bool,
        count :: Int,
        accumulatedString :: String
    }

beginTuring :: Tape a -> TransitionTable a Direction -> State -> TuringMachine a
beginTuring tape transTable state = TM tape transTable state False 0 ""

instance (Show a) => Show (TuringMachine a) where
    show (TM _ _ st h c str) = str
                               ++ "\nSteps: " ++ show c
                               ++ "\nAccepted: " ++ show (h && isAccept st)

showTapeState :: (Show a) => TuringMachine a -> String
showTapeState tm = show (tape tm) ++ " " ++ show (currentState tm)

tmPerformAction :: TuringMachine a -> Action a Direction -> TuringMachine a
tmPerformAction tm Fail = tm { halt = True }
tmPerformAction tm@(TM t _ _ _ c _) (Action nxt wChar dir)
                    = tm {
                        tape = tapeShift (tapeWrite t wChar) dir,
                        currentState = nxt,
                        count = c + 1}

tmStep :: (Eq a, Show a) => TuringMachine a -> [TuringMachine a]
tmStep tm@(TM _ _ _ True _ _) = pure tm
tmStep tm = map ((\x -> x{accumulatedString = accumulatedString x ++ "\n" ++ showTapeState tm} ) 
                . tmPerformAction tm) actions
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
tmRun tm@(TM _ _ _ True _ _) = Just tm 
tmRun tm = result
    where result = tmRun' [tm]

tmControlledRun :: (Eq a, Show a) => Int -> TuringMachine a -> Maybe (TuringMachine a)
tmControlledRun _ tm@(TM _ _ _ True _ _) = Just tm
tmControlledRun n tm = result
    where result = tmControlledRun' n [tm]