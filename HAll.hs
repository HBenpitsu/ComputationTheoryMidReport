import Lib.Definition
import Lib.HeadActions

-- tmAll :: TuringMachine
-- tmAll = TM {
--     externalTapes = 1,
--     initInnerTapes = [],
--     transition = \_ [ipt] -> HALT_TR ACCEPT
-- }
-- tmEmpty :: TuringMachine
-- tmEmpty = TM {
--     externalTapes = 1,
--     initInnerTapes = [],
--     transition = \_ [ipt] -> HALT_TR REJECT
-- }

f :: TuringMachine
f = TM {
    externalTapes = 3,
    initInnerTapes = [],
    transition = \lastCall [ENCODED_TM machine, word, return] ->
        let returnTM tm = MODS_TR [
                identTape,
                identTape,
                writeAtHead (ENCODED_TM tm)
                ]
        in case return of
            BLANK -> MODS_TR [
                identTape,
                identTape,
                writeAtHead (ENCODED_TM TM {
                    externalTapes = 1,
                    initInnerTapes = [TAPE 0 []],
                    transition = \lastCall' [_, word'] -> case lastCall' of
                        NO_CALL_JUST_BEFORE -> CALL_TR machine [1]
                        CALL_JUST_BEFORE ACCEPT -> HALT_TR ACCEPT
                        CALL_JUST_BEFORE REJECT -> HALT_TR REJECT
                })]
            ENCODED_TM TM {
                externalTapes = externalTapes',
                initInnerTapes = [TAPE 0 word'],
                transition = transition'
            } -> case word of
                BLANK -> HALT_TR ACCEPT
                _ -> MODS_TR [
                    identTape,
                    moveHead RIGHT,
                    writeAtHead (ENCODED_TM TM {
                        externalTapes = externalTapes',
                        -- accumulate the word on the tape
                        initInnerTapes = [TAPE 0 (word' ++ [word])],
                        transition = transition'
                    })]

}

-- | impossible
hAll :: TuringMachine
hAll = TM {
    externalTapes = 1
}

hAcc :: TuringMachine
hAcc = TM {
    externalTapes = 2,
    initInnerTapes = [TAPE 0 [BLANK]],
    transition = \lastCall [tm, word, mAll] ->
        case mAll of
            BLANK -> case tm of
                ENCODED_TM tm -> CALL_TR f [0, 1, 2] 
                _             -> HALT_TR REJECT
            ENCODED_TM _ -> case lastCall of
                NO_CALL_JUST_BEFORE -> CALL_TR hAll [2]
                CALL_JUST_BEFORE ACCEPT -> HALT_TR ACCEPT
                CALL_JUST_BEFORE REJECT -> HALT_TR REJECT
            _ -> error "Unexpected tape content"
            
}