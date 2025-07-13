module Lib.Utilities where

import Lib.Definition

toTape :: String -> Tape
toTape str = if null str 
    then TAPE 0 [BLANK] 
    else TAPE 0 (map SYM str)