module Lib.Module where

import Lib.Definition
import Lib.HeadActions

-- | Copy a tape to another tape. The first tape is the source, the second tape is the destination.
tmCopy :: TuringMachine
tmCopy = TM {
    externalTapes = 2,
    initInnerTapes = [TAPE 0 [MODE "COPY"]],
    transition = \lastCall [from, to, mode] -> 
        case mode of
            MODE "COPY" -> case from of
                BLANK -> MODS_TR [
                        moveHead LEFT,
                        moveHead LEFT,
                        writeAtHead $ MODE "BACK"
                    ]
                cell -> MODS_TR [
                        moveHead RIGHT,
                        moveHead RIGHT . writeAtHead cell,
                        identTape
                    ]
            MODE "BACK" -> case from of
                BLANK -> MODS_TR [
                    moveHead RIGHT,
                    moveHead RIGHT,
                    writeAtHead (MODE "TERMINATE")
                    ]
                _ -> MODS_TR [
                    moveHead LEFT,
                    moveHead LEFT,
                    identTape
                    ]
            MODE "TERMINATE" -> HALT_TR ACCEPT
}