{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Format.Ident where

import           Language.PureScript.Names

import           Data.Text.Prettyprint.Doc

import           Data.Text                 (Text, unpack)

instance Pretty Ident where
    pretty :: Ident -> Doc ann
    pretty (Ident t) = pretty t
    pretty _         = error "unhandled ident"

