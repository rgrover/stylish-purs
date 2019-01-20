{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Format.Comment where

import           Language.PureScript.Comments
import           Language.PureScript.Names

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Util

instance Pretty Comment where
    pretty :: Comment -> Doc ann
    pretty (LineComment t)  = "--" <> pretty t
    pretty (BlockComment t) = "{-" <+> nest 3 (reflow t <+> "-}")
