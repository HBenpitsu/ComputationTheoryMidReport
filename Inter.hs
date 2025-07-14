import Lib.Definition
import Lib.HeadActions
import Lib.Utilities
import Lib.Processor
import Lib.Module (tmCopy, makeDummyTM)

m1 = makeDummyTM "abc"
m2 = makeDummyTM "ade"

tmInter :: TuringMachine -> TuringMachine -> TuringMachine
tmInter m1 m2 = TM {
    externalTapes = 1,
    initInnerTapes = [TAPE 0 [BLANK], TAPE 0 [BLANK], TAPE 0 [MODE "INIT1"]],
    transition = \lastCall [ipt, iptBuf1, iptBuf2, mode] ->
        let changeMode str = MODS_TR [
                identTape,
                identTape,
                identTape,
                writeAtHead (MODE str)
                ]
        in case mode of
            MODE "INIT1" -> case lastCall of
                -- cp: ipt -> iptBuf1
                NO_CALL_JUST_BEFORE -> CALL_TR tmCopy [0, 1]
                CALL_JUST_BEFORE ACCEPT -> changeMode "INIT2"
            MODE "INIT2" -> case lastCall of
                -- cp: ipt -> iptBuf2
                NO_CALL_JUST_BEFORE -> CALL_TR tmCopy [0, 2]
                CALL_JUST_BEFORE ACCEPT -> changeMode "M1"
            MODE "M1" -> case lastCall of
                -- m1(iptBuf1)
                NO_CALL_JUST_BEFORE -> CALL_TR m1 [1]
                CALL_JUST_BEFORE REJECT -> HALT_TR REJECT
                CALL_JUST_BEFORE ACCEPT -> changeMode "M2"
            MODE "M2" -> case lastCall of
                -- m2(iptBuf2)
                NO_CALL_JUST_BEFORE -> CALL_TR m2 [2]
                CALL_JUST_BEFORE REJECT -> HALT_TR REJECT
                CALL_JUST_BEFORE ACCEPT -> HALT_TR ACCEPT
}

main :: IO ()
main = do
    putStrLn "Inter Machine:"
    putStrLn $ case process (tmInter m1 m2) [toTape "abcde"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes
    putStrLn $ case process (tmInter m1 m2) [toTape "abc"] of
        (ACCEPT, tapes) -> "Accepted: " ++ show tapes
        (REJECT, tapes) -> "Rejected: " ++ show tapes