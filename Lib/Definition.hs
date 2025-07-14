module Lib.Definition where
import Lib.ListOps (insert)

{- TAPE -}
-- caution: Althogh domain of mode is infinite, it should be treated as finite.
-- MODE is mere syntax sugar.
-- Domain of TapeCell should be finite.
data TapeCell = SYM Char | MODE String | ENCODED_TM TuringMachine | BLANK
instance Show TapeCell where
    show (SYM ch) = [ch]
    show (MODE str) = "[" ++ str ++ "]"
    show (ENCODED_TM _) = "<TM>"
    show BLANK = " "
instance Eq TapeCell where
    (SYM a) == (SYM b) = a == b
    (MODE a) == (MODE b) = a == b
    BLANK == BLANK = True
    -- Giving up to compare TuringMachines
    -- Logically, equality of TuringMachines is decidable
    (ENCODED_TM _) == (ENCODED_TM _) = False
    _ == _ = False
type HeadIdx = Int
data Tape = TAPE HeadIdx [TapeCell]
instance Show Tape where
    show (TAPE idx cells) = insert idx '|' (concatMap show cells)
{- Turing Machine -}
data Halt = ACCEPT | REJECT deriving (Show, Eq)
type TuringMachineResult = (Halt, [Tape])
type TapeModifier = Tape -> Tape
data TransitionResult =
      MODS_TR [TapeModifier]
    | CALL_TR TuringMachine [Int]
    | HALT_TR Halt
-- TransitionFunction(lastCallResult, currentTapes) -> TransitionResult
data LastCallResult = NO_CALL_JUST_BEFORE | CALL_JUST_BEFORE Halt deriving (Show, Eq)
type TransitionFunction = LastCallResult -> [TapeCell] -> TransitionResult

data TuringMachine = TM {
    externalTapes :: Int,
    initInnerTapes :: [Tape],
    transition :: TransitionFunction
}

