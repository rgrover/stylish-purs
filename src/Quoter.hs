module Quoter (fmt, format) where

import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (liftData)

import           Format.Formatter

import           Data.Text                  hiding (head, tail)

fmt :: QuasiQuoter
fmt = QuasiQuoter {
      quoteExp  = liftData . format
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

