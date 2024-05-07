
type State = String

type Symbol = Char

-- Some variants support a "no move"(N) instruction
data Direction = L | R 

-- Rule is written as a function
type Rule = (State, Symbol) -> (State, Symbol, Direction)

type TuringMachine = (State, Tape)

-- The left side of the tape is reversed (for convenience of computation)
data Tape = Tape [Symbol] Symbol [Symbol]
instance Show Tape where
    show (Tape ls n rs) = show (reverse ls) ++ " [" ++ show n ++ "] " ++ show rs

moveTape :: Direction -> Tape -> Tape
moveTape L (Tape (l:ls) n rs) =
    case ls of  -- extend the left since TM has infinite tape
        [] -> Tape ['□'] l (n:rs)
        _  -> Tape ls l (n:rs)
moveTape R (Tape ls n (r:rs)) =
    case rs of  -- extend the right since TM has infinite tape
        [] -> Tape (n:ls) r ['□']
        _  -> Tape (n:ls) r rs

readTape :: Tape -> Symbol
readTape (Tape _ n _) = n

abstractTuringMaching :: Rule -> TuringMachine -> TuringMachine
abstractTuringMaching rules (st, tape)
    = (newState, moveTape newDirect tape)
    where 
        (newState, newSymbol, newDirect) = rules (st, readTape tape)

runTuringMachine :: Rule -> TuringMachine -> TuringMachine
runTuringMachine rule machine = 
    case newState of
        "HALT" -> (newState, newTape)
        _      -> runTuringMachine rule (newState, newTape)
    where (newState, newTape) = abstractTuringMaching rule machine

threeStateBusyBeaver :: Rule
threeStateBusyBeaver state =
    case state of 
    ("A", '0') -> ("B", '1', R)
    ("A", '1') -> ("C", '1', L)
    ("B", '0') -> ("A", '1', L)
    ("B", '1') -> ("B", '1', R)
    ("C", '0') -> ("B", '1', L)
    ("C", '1') -> ("HALT", '1', R)

threeStateBusyBeaverMachine :: TuringMachine
threeStateBusyBeaverMachine = ("C", Tape ['0'] '0' ['0', '0', '0', '0'])

-- palindromeRule :: Rule
-- palindromeRule state =
--     case state of 
--     ("Q0", '')

