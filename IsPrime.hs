import Lib.Definition
import Lib.HeadActions
import Lib.Utilities
import Lib.Processor
import Lib.Module
import Data.Char (digitToInt, intToDigit, isDigit)

-- | Compare two tapes and ACCEPT when they are identical, REJECT otherwise.
tmEqual :: TuringMachine
tmEqual = TM {
    externalTapes = 2,
    initInnerTapes = [],
    transition = \lastCall [ipt1, ipt2] ->
        case (ipt1, ipt2) of
            (BLANK, BLANK)  -> HALT_TR ACCEPT
            (SYM c1, SYM c2)-> 
                if c1 == c2
                    then MODS_TR [moveHead RIGHT, moveHead RIGHT] 
                    else HALT_TR REJECT
            _               -> HALT_TR REJECT
}

-- | Reverse a given tape. Its head is at the front of the tape after the process.
tmReverse :: TuringMachine
tmReverse = TM {
    externalTapes = 1,
    initInnerTapes = [TAPE 0 [BLANK], TAPE 0 [MODE "GO_RIGHT"]],
    transition = \lastCall [ipt, buf, mode] ->
        case mode of
            MODE "GO_RIGHT" -> if ipt == BLANK 
                then MODS_TR [
                    moveHead LEFT,
                    moveHead LEFT,
                    writeAtHead (MODE "INIT_BUF_CUR")
                ]
                else MODS_TR [
                    moveHead RIGHT, 
                    moveHead RIGHT . writeAtHead ipt, 
                    identTape
                ]
            MODE "INIT_BUF_CUR" -> if buf == BLANK
                then MODS_TR [
                    identTape,
                    moveHead RIGHT,
                    writeAtHead (MODE "OVERWRITE")
                ]
                else MODS_TR [
                    identTape,
                    moveHead LEFT,
                    identTape
                ]
            MODE "OVERWRITE" -> if buf == BLANK
                then MODS_TR [
                    moveHead RIGHT,
                    identTape,
                    writeAtHead (MODE "TERMINATE")
                ]
                else MODS_TR [
                    moveHead LEFT . writeAtHead buf,
                    moveHead RIGHT,
                    identTape
                ]
            MODE "TERMINATE" -> HALT_TR ACCEPT
}

_blankOrDigit :: TapeCell -> Bool
_blankOrDigit BLANK = True
_blankOrDigit (SYM c) = isDigit c
_blankOrDigit _ = False

-- | Take two tapes that contain natural numbers in base 10.
-- this rewrites the first tape to the sum of the two numbers.
-- REJECT if the given tapes contain invalid numbers.
tmAdd :: TuringMachine
tmAdd = TM {
    externalTapes = 2,
    initInnerTapes = [
        TAPE 0 [BLANK], 
        TAPE 0 [MODE "REV1"]
    ],
    transition = \lastCall [ipt1, ipt2, carry, mode] ->
        let changeMode mode = MODS_TR [
                identTape,
                identTape,
                identTape,
                writeAtHead (MODE mode)
                ]
        in case mode of
            MODE "REV1" -> case lastCall of
                -- rev ipt1
                NO_CALL_JUST_BEFORE     -> CALL_TR tmReverse [0]
                CALL_JUST_BEFORE ACCEPT -> changeMode "REV2"
            MODE "REV2" -> case lastCall of
                -- rev ipt2
                NO_CALL_JUST_BEFORE     -> CALL_TR tmReverse [1]
                CALL_JUST_BEFORE ACCEPT -> changeMode "ADD"
            MODE "ADD" -> case (ipt1, ipt2, carry) of
                (BLANK, BLANK, SYM '0') -> MODS_TR [
                    moveHead LEFT,
                    moveHead LEFT,
                    identTape,
                    writeAtHead (MODE "TERMINATE_LEF")
                    ]
                (x, y, z) ->
                    if _blankOrDigit x && _blankOrDigit y && _blankOrDigit z
                        then let 
                            SYM x' = if x == BLANK then SYM '0' else x
                            SYM y' = if y == BLANK then SYM '0' else y
                            SYM z' = if z == BLANK then SYM '0' else z
                            -- Although it can be written as a table,
                            -- It is too large to write as code.
                            sum = digitToInt x' + digitToInt y' + digitToInt z'
                            sum' = SYM $ intToDigit $ sum `mod` 10
                            carry' = SYM $ intToDigit $ sum `div` 10
                            in MODS_TR [
                                moveHead RIGHT . writeAtHead sum',
                                moveHead RIGHT,
                                writeAtHead carry' . moveHead RIGHT,
                                writeAtHead (MODE "ADD")
                            ]
                        else changeMode "TERMINATE_LEF_REJ"
            -- accept after all
            MODE "TERMINATE_LEF" -> case (ipt1, ipt2) of
                (BLANK, BLANK) -> MODS_TR [
                    moveHead RIGHT,
                    moveHead RIGHT,
                    identTape,
                    writeAtHead (MODE "TERMINATE_REV1")
                    ]
                (_, _) -> MODS_TR [
                    moveHead LEFT,
                    moveHead LEFT,
                    identTape,
                    identTape
                    ]
            MODE "TERMINATE_REV1" -> case lastCall of
                -- rev ipt1
                NO_CALL_JUST_BEFORE     -> CALL_TR tmReverse [0]
                CALL_JUST_BEFORE ACCEPT -> changeMode "TERMINATE_REV2" 
            MODE "TERMINATE_REV2" -> case lastCall of
                -- rev ipt2
                NO_CALL_JUST_BEFORE     -> CALL_TR tmReverse [1]
                CALL_JUST_BEFORE ACCEPT -> HALT_TR ACCEPT

            -- reject after all
            MODE "TERMINATE_LEF_REJ" -> case (ipt1, ipt2) of
                (BLANK, BLANK) -> MODS_TR [
                    moveHead RIGHT,
                    moveHead RIGHT,
                    identTape,
                    writeAtHead (MODE "TERMINATE_REV1_REJ")
                    ]
                (_, _) -> MODS_TR [
                    moveHead LEFT,
                    moveHead LEFT,
                    identTape,
                    identTape
                    ]
            MODE "TERMINATE_REV1_REJ" -> case lastCall of
                -- rev ipt1
                NO_CALL_JUST_BEFORE     -> CALL_TR tmReverse [0]
                CALL_JUST_BEFORE ACCEPT -> changeMode "TERMINATE_REV2_REJ" 
            MODE "TERMINATE_REV2_REJ" -> case lastCall of
                -- rev ipt2
                NO_CALL_JUST_BEFORE     -> CALL_TR tmReverse [1]
                CALL_JUST_BEFORE ACCEPT -> HALT_TR REJECT
}


-- | Take two tapes that contain natural numbers in base 10.
-- this rewrites the first tape to the sum of the two numbers.
-- REJECT if the given tapes contain invalid numbers.
-- Or REJECT if the first number is smaller than the second number.
tmSub :: TuringMachine
tmSub = TM {
    externalTapes = 2,
    initInnerTapes = [
        TAPE 0 [BLANK], 
        TAPE 0 [MODE "REV1"]
    ],
    transition = \lastCall [ipt1, ipt2, carry, mode] ->
        let changeMode mode = MODS_TR [
                identTape,
                identTape,
                identTape,
                writeAtHead (MODE mode)
                ]
        in case mode of
            MODE "REV1" -> case lastCall of
                -- rev ipt1
                NO_CALL_JUST_BEFORE     -> CALL_TR tmReverse [0]
                CALL_JUST_BEFORE ACCEPT -> changeMode "REV2"
            MODE "REV2" -> case lastCall of
                -- rev ipt2
                NO_CALL_JUST_BEFORE     -> CALL_TR tmReverse [1]
                CALL_JUST_BEFORE ACCEPT -> changeMode "SUB"
            MODE "SUB" -> case (ipt1, ipt2, carry) of
                (BLANK, BLANK, SYM '0') -> MODS_TR [
                    moveHead LEFT,
                    moveHead LEFT,
                    identTape,
                    writeAtHead (MODE "TERMINATE_LEF")
                    ]
                (BLANK, BLANK, SYM '1') -> MODS_TR [
                    moveHead LEFT,
                    moveHead LEFT,
                    identTape,
                    writeAtHead (MODE "TERMINATE_LEF_REJ")
                    ]
                (x, y, z) -> 
                    if _blankOrDigit x && _blankOrDigit y && _blankOrDigit z
                        then let 
                            SYM x' = if x == BLANK then SYM '0' else x
                            SYM y' = if y == BLANK then SYM '0' else y
                            SYM z' = if z == BLANK then SYM '0' else z
                            -- Although it can be written as a table,
                            -- It is too large to write as code.
                            sum = digitToInt x' - digitToInt y' - digitToInt z'
                            sum' = SYM $ intToDigit $ if sum < 0 then sum + 10 else sum
                            carry' = SYM $ if sum < 0 then '1' else '0'
                        in MODS_TR [
                            moveHead RIGHT . writeAtHead sum',
                            moveHead RIGHT,
                            writeAtHead carry' . moveHead RIGHT,
                            writeAtHead (MODE "SUB")
                        ]
                        else changeMode "TERMINATE_LEF_REJ"
            
            -- accept after all
            MODE "TERMINATE_LEF" -> case (ipt1, ipt2) of
                (BLANK, BLANK) -> MODS_TR [
                    moveHead RIGHT,
                    moveHead RIGHT,
                    identTape,
                    writeAtHead (MODE "TERMINATE_REV1")
                    ]
                (_, _) -> MODS_TR [
                    moveHead LEFT,
                    moveHead LEFT,
                    identTape,
                    identTape
                    ]
            MODE "TERMINATE_REV1" -> case lastCall of
                -- rev ipt1
                NO_CALL_JUST_BEFORE     -> CALL_TR tmReverse [0]
                CALL_JUST_BEFORE ACCEPT -> changeMode "TERMINATE_REV2" 
            MODE "TERMINATE_REV2" -> case lastCall of
                -- rev ipt2
                NO_CALL_JUST_BEFORE     -> CALL_TR tmReverse [1]
                CALL_JUST_BEFORE ACCEPT -> HALT_TR ACCEPT
            
            -- reject after all
            MODE "TERMINATE_LEF_REJ" -> case (ipt1, ipt2) of
                (BLANK, BLANK) -> MODS_TR [
                    moveHead RIGHT,
                    moveHead RIGHT,
                    identTape,
                    writeAtHead (MODE "TERMINATE_REV1_REJ")
                    ]
                (_, _) -> MODS_TR [
                    moveHead LEFT,
                    moveHead LEFT,
                    identTape,
                    identTape
                    ]
            MODE "TERMINATE_REV1_REJ" -> case lastCall of
                -- rev ipt1
                NO_CALL_JUST_BEFORE     -> CALL_TR tmReverse [0]
                CALL_JUST_BEFORE ACCEPT -> changeMode "TERMINATE_REV2_REJ" 
            MODE "TERMINATE_REV2_REJ" -> case lastCall of
                -- rev ipt2
                NO_CALL_JUST_BEFORE     -> CALL_TR tmReverse [1]
                CALL_JUST_BEFORE ACCEPT -> HALT_TR REJECT
}

tmIsZero :: TuringMachine
tmIsZero = TM {
    externalTapes = 1,
    initInnerTapes = [],
    transition = \lastCall [ipt] ->
        case ipt of
            BLANK -> HALT_TR ACCEPT
            SYM '0' -> MODS_TR [moveHead RIGHT]
            SYM _ -> HALT_TR REJECT
}

tmIncrement :: TuringMachine
tmIncrement = TM {
    externalTapes = 1,
    initInnerTapes = [TAPE 0 [SYM '1']],
    transition = \lastCall [ipt, const1] -> 
        case lastCall of
            NO_CALL_JUST_BEFORE     -> CALL_TR tmAdd [0, 1]
            CALL_JUST_BEFORE ACCEPT -> HALT_TR ACCEPT
            CALL_JUST_BEFORE REJECT -> HALT_TR REJECT
}

tmMod :: TuringMachine
tmMod = TM {
    externalTapes = 2,
    initInnerTapes = [
        TAPE 0 [BLANK],
        TAPE 0 [MODE "INIT"]
    ],
    transition = \lastCall [ipt1, ipt2, ipt1buf, mode] -> 
        let changeMode mode = MODS_TR [
                identTape,
                identTape,
                identTape,
                writeAtHead (MODE mode)
                ]
        in case mode of
        MODE "INIT" -> case lastCall of
            -- cp: ipt1 -> ipt1buf
            NO_CALL_JUST_BEFORE     -> CALL_TR tmCopy [0, 2]
            CALL_JUST_BEFORE ACCEPT -> changeMode "SUB"
        MODE "SUB" -> case lastCall of
            -- ipt1buf -= ipt2
            NO_CALL_JUST_BEFORE     -> CALL_TR tmSub [2, 1]
            CALL_JUST_BEFORE REJECT -> HALT_TR ACCEPT
            CALL_JUST_BEFORE ACCEPT -> changeMode "CPY"
        MODE "CPY" -> case lastCall of
            -- cp: ipt1buf -> ipt1 / reflect tentative result
            NO_CALL_JUST_BEFORE     -> CALL_TR tmCopy [2, 0]
            CALL_JUST_BEFORE ACCEPT -> changeMode "SUB"
}

tmIsPrime :: TuringMachine
tmIsPrime = TM {
    externalTapes = 1,
    initInnerTapes = [
        TAPE 0 [BLANK],
        TAPE 0 [SYM '2'],
        TAPE 0 [MODE "INIT"]
    ],
    transition = \lastCall [ipt, iptBuf, factor, mode] -> 
        let changeMode mode = MODS_TR [
                identTape,
                identTape,
                identTape,
                writeAtHead (MODE mode)
                ]
        in case mode of
            MODE "INIT" -> case lastCall of
                -- cp: ipt -> iptBuf
                NO_CALL_JUST_BEFORE     -> CALL_TR tmCopy [0, 1]
                CALL_JUST_BEFORE ACCEPT -> changeMode "LESS_THAN_2"
            MODE "LESS_THAN_2" -> case lastCall of
                -- iptBuf < factor(2) -> reject
                NO_CALL_JUST_BEFORE     -> CALL_TR tmSub [1, 2]
                CALL_JUST_BEFORE REJECT -> HALT_TR REJECT
                CALL_JUST_BEFORE ACCEPT -> changeMode "RESTORE_IPT"
            MODE "RESTORE_IPT" -> case lastCall of
                -- cp: ipt -> iptBuf
                NO_CALL_JUST_BEFORE     -> CALL_TR tmCopy [0, 1]
                CALL_JUST_BEFORE ACCEPT -> changeMode "LESS_THAN_FACTOR"
            MODE "LESS_THAN_FACTOR" -> case lastCall of
                -- iptBuf > factor -> accept
                NO_CALL_JUST_BEFORE     -> CALL_TR tmSub [1, 2]
                CALL_JUST_BEFORE REJECT -> HALT_TR ACCEPT
                CALL_JUST_BEFORE ACCEPT -> changeMode "LESS_THAN_EQ_FACTOR"
            MODE "LESS_THAN_EQ_FACTOR" -> case lastCall of
                -- iptBuf == factor -> accept
                NO_CALL_JUST_BEFORE     -> CALL_TR tmIsZero [1]
                CALL_JUST_BEFORE ACCEPT -> HALT_TR ACCEPT
                CALL_JUST_BEFORE REJECT -> changeMode "RESTORE_IPT_MOD"
            MODE "RESTORE_IPT_MOD" -> case lastCall of
                -- cp: ipt -> iptBuf
                NO_CALL_JUST_BEFORE     -> CALL_TR tmCopy [0, 1]
                CALL_JUST_BEFORE ACCEPT -> changeMode "MOD"
            MODE "MOD" -> case lastCall of
                -- iptBuf %= factor
                NO_CALL_JUST_BEFORE     -> CALL_TR tmMod [1, 2]
                CALL_JUST_BEFORE REJECT -> HALT_TR REJECT
                CALL_JUST_BEFORE ACCEPT -> changeMode "MOD_IS_ZERO"
            MODE "MOD_IS_ZERO" -> case lastCall of
                -- iptBuf == 0 -> reject
                NO_CALL_JUST_BEFORE     -> CALL_TR tmIsZero [1]
                CALL_JUST_BEFORE ACCEPT -> HALT_TR REJECT
                CALL_JUST_BEFORE REJECT -> changeMode "INCREMENT_FACTOR"
            MODE "INCREMENT_FACTOR" -> case lastCall of
                -- factor++
                NO_CALL_JUST_BEFORE     -> CALL_TR tmIncrement [2]
                CALL_JUST_BEFORE REJECT -> HALT_TR REJECT
                CALL_JUST_BEFORE ACCEPT -> changeMode "RESTORE_IPT_LTF"
            MODE "RESTORE_IPT_LTF" -> case lastCall of
                -- cp: ipt -> iptBuf
                NO_CALL_JUST_BEFORE     -> CALL_TR tmCopy [0, 1]
                CALL_JUST_BEFORE ACCEPT -> changeMode "LESS_THAN_FACTOR"
}     

main :: IO()
main = do
    putStrLn $ case process tmAdd [toTape "", toTape ""] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmAdd [toTape "9020", toTape "1280"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmAdd [toTape "x9020", toTape "1280"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmSub [toTape "10300", toTape "1280"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmSub [toTape "1280", toTape "10300"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmSub [toTape "", toTape ""] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmSub [toTape "10", toTape "-6"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmMod [toTape "5", toTape "10"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmMod [toTape "25", toTape "10"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn "isPrime:"
    putStrLn $ case process tmIsPrime [toTape "0"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmIsPrime [toTape "1"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmIsPrime [toTape "2"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmIsPrime [toTape "3"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmIsPrime [toTape "4"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmIsPrime [toTape "5"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmIsPrime [toTape "6"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmIsPrime [toTape "19"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmIsPrime [toTape "20"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmIsPrime [toTape "21"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmIsPrime [toTape "22"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process tmIsPrime [toTape "23"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
