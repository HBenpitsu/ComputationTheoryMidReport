module Lib.Processor where

import Lib.Definition
import Lib.ListOps (indexOf)
import Lib.HeadActions (cellsAtHeads)

{- |
return tapes they are corresponding to given tapes.
Their order is preserved. 
-}
process :: TuringMachine -> [Tape] -> TuringMachineResult
process tm tapes =
    let externalTapes' = externalTapes tm
        initInnerTapes' = initInnerTapes tm
        transition' = transition tm
        validate = if externalTapes' /= length tapes
            then error $ "Expected " ++ show externalTapes' ++ 
                         " tapes, but got " ++ show (length tapes)
            else "valid"
        process' lastCallResult tapes = 
            case transition' lastCallResult (cellsAtHeads tapes) of
            MODS_TR mods -> process' NO_CALL_JUST_BEFORE (zipWith ($) mods tapes)
            CALL_TR callee tapeIdxs -> 
                let sharedTapes' = [tapes !! i | i <- tapeIdxs]
                    (halt, processedTapes) = process callee sharedTapes'
                    newTapes = [ 
                        if i `elem` tapeIdxs 
                            then case indexOf i tapeIdxs of 
                                Just j -> processedTapes !! j
                                Nothing -> error "Index out of bounds"
                            else tapes !! i
                        | i <- [0..(length tapes - 1)] 
                        ]
                in process' (CALL_JUST_BEFORE halt) newTapes
            HALT_TR halt   -> (halt, tapes)
        (halt, processedTapes) = process' NO_CALL_JUST_BEFORE (tapes ++ initInnerTapes')
    in (halt, take externalTapes' processedTapes)
