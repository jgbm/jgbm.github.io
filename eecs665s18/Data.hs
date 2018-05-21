module AsmProg where

import X86
import Text
import Machine
import Loader

import Data.Array.IArray

program =
 [global "main"
   [movq ~#"str" ~%RAX,
    movq ~#"word" ~%RCX,
    jmp ~$$"main"],
  ("str", True, Data [String "ab"]),
  ("word", True, Data [Word (Literal 9223372036854775807)])]
