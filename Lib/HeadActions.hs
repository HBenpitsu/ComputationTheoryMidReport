module Lib.HeadActions where

import Lib.Definition
import Lib.ListOps (replace, indexOf)

-- take cell at head
_cellAtHead :: Tape -> TapeCell
_cellAtHead (TAPE idx cells) =
    if idx < 0 || idx >= length cells
        then error ("Head index out of bounds: " ++ show (TAPE idx cells))
        else cells !! idx
cellsAtHeads :: [Tape] -> [TapeCell]
cellsAtHeads = map _cellAtHead
-- set cell at head
writeAtHead :: TapeCell -> Tape -> Tape
writeAtHead newCell (TAPE idx cells) =
    if idx < 0 || idx >= length cells
        then error ("Head index out of bounds: " ++ show (TAPE idx cells))
        else TAPE idx (replace idx newCell cells)
data Direction = LEFT | RIGHT deriving (Show, Eq)
-- move head, dual-infinite tape
moveHead :: Direction -> Tape -> Tape
moveHead dir (TAPE idx cells) =
    let newIdx = case dir of
            LEFT  -> max 0 (idx - 1)
            RIGHT -> idx + 1
        headCells = [BLANK | idx == 0 && dir == LEFT]
        tailCells = [BLANK | idx == length cells - 1 && dir == RIGHT]
        crop idx (head:rest)
            | idx == 0      = (0, head:rest)
            | head == BLANK = (idx-1, rest)
            | otherwise     = (idx, head:rest)
        (newIdx', cells') = crop newIdx (headCells ++ cells ++ tailCells)
    in TAPE newIdx' cells'
-- keep tape as is
identTape :: Tape -> Tape
identTape tape = tape