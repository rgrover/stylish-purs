{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Format.EitherPrettyOrErrors where

import           Data.Text.Prettyprint.Doc
import           Data.Validation

import           Language.PureScript.AST.SourcePos

newtype UnhandledError = UnhandledError (SourceSpan, String)
type Output ann        = Validation [UnhandledError] (Doc ann)

class EitherPrettyOrErrors a where
    prettyE :: SourceSpan -> a -> Output ann

instance Pretty UnhandledError where
    pretty (UnhandledError (span, msg)) =
        "stylish-purs: line:" <> pretty line <> ", column:" <>
        pretty column <> " :: " <> pretty msg
      where
        pos    = spanStart span
        line   = sourcePosLine pos
        column = sourcePosColumn pos
