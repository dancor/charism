{-# LANGUAGE TemplateHaskell #-}

module Opt where

import Data.PolyOpt
import WordOrder

readWdOrd :: String -> WordOrder
readWdOrd "a" = Alph
readWdOrd "r" = Rand
readWdOrd x = error $ "invalid word order (should be 'a' or 'r'): " ++ x

$(polyOpt [
  reqArgGen ["max-word-len"] "n"
    "N" [t|Int|] [|7|] [|read|]
    "Maximum word length",
  reqArgGen ["min-word-len"] "m"
    "N" [t|Int|] [|3|] [|read|]
    "Minimum word length",
  reqArgGen ["word-order"] "m"
    "a|r" [t|WordOrder|] [|Alph|] [|readWdOrd|]
    "Word order (alphabetical|random)"
  ])

