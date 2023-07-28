module TuringMachine(TuringMachine(..), tmRun, beginTuring) where
import Tape.Tape
import State.Transition

data TuringMachine a = TM {
        tape :: Tape a,
        transitionTable :: TransitionTable a,
        currentState :: State,
        halt :: Bool,
        count :: Int,
        accumulatedString :: String
    }

beginTuring :: Tape a -> TransitionTable a -> State -> TuringMachine a
beginTuring tape transTable state = TM tape transTable state False 0 ""

instance (Show a) => Show (TuringMachine a) where
    show (TM _ _ st h c str) = str
                               ++ "\nSteps: " ++ show c
                               ++ "\nAccepted: " ++ show (h && isAccept st)

showTapeState :: (Show a) => TuringMachine a -> String
showTapeState tm = show (tape tm) ++ " " ++ show (currentState tm)

tmPerformAction :: TuringMachine a -> Action a -> TuringMachine a
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
tmRun' [] = Nothing
tmRun' tms = if not (null successful)
             then Just . head $ successful
             else tmRun' continue
       where
            onNext = concatMap tmStep tms
            hadStopped = filter halt onNext
            continue = filter (not . halt) onNext
            successful = filter ( isAccept . currentState ) hadStopped

tmRun :: (Eq a, Show a) => TuringMachine a -> IO ()
tmRun (TM _ _ _ True _ _) = print "Machine has already halted"
tmRun tm = maybe (print "Paniquei") print result
    where result = tmRun' [tm]