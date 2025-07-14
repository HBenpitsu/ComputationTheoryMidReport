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
moveHead dir (TAPE idx cells) = case (idx == 0, idx == length cells - 1, dir) of
            (True, False, RIGHT) 
                | head cells == BLANK -> TAPE 0 (tail cells)
            (False, True, LEFT) 
                | last cells == BLANK -> TAPE (idx - 1) (init cells)
            (True, _, LEFT)           -> TAPE 0 (BLANK : cells)
            (_, True, RIGHT)          -> TAPE (idx + 1) (cells ++ [BLANK])
            (_, _, _)                 -> TAPE (idx + if dir == LEFT then -1 else 1) cells
-- keep tape as is
identTape :: Tape -> Tape
identTape tape = tape