import Lib.Definition
import Lib.HeadActions
import Lib.Module
import Lib.Processor
import Lib.Utilities

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
                moveHead RIGHT,
                writeAtHead (ENCODED_TM TM {
                    externalTapes = 1,
                    initInnerTapes = [TAPE 0 [word]],
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
    externalTapes = 1,
    -- dummy
    initInnerTapes = [TAPE 0 [BLANK]],
    transition = \lastCall [ENCODED_TM ipt, tester] -> 
        case lastCall of 
            NO_CALL_JUST_BEFORE -> CALL_TR ipt [1]
            CALL_JUST_BEFORE ACCEPT -> HALT_TR ACCEPT
            CALL_JUST_BEFORE REJECT -> HALT_TR REJECT
}

hAcc :: TuringMachine
hAcc = TM {
    externalTapes = 2,
    initInnerTapes = [TAPE 0 [BLANK]],
    transition = \lastCall [tm, word, mAll] ->
        case mAll of
            BLANK -> case tm of
                -- mAll = f(tm, word)
                ENCODED_TM tm -> CALL_TR f [0, 1, 2] 
                _             -> HALT_TR REJECT
            ENCODED_TM _ -> case lastCall of
                -- hAll(mAll)
                NO_CALL_JUST_BEFORE -> CALL_TR hAll [2]
                CALL_JUST_BEFORE halt -> HALT_TR halt
            _ -> error "Unexpected tape content"           
}

m = makeDummyTM "abc"

main :: IO ()
main = do 
    putStrLn "hAcc:"
    putStrLn $ case process hAcc [TAPE 0 [ENCODED_TM m], toTape "abc"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process hAcc [TAPE 0 [ENCODED_TM m], toTape "e"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes