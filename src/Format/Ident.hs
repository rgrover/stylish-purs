{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Format.Ident where

import           Data.Text.Prettyprint.Doc
import           Language.PureScript.Names

instance Pretty Ident where
    pretty :: Ident -> Doc ann
    pretty (Ident t) = pretty t
    pretty _         = error "unhandled ident"

instance Pretty a => Pretty (Qualified a) where
    pretty :: Qualified a -> Doc ann
    pretty (Qualified optionalModuleName i) =
        case runModuleName <$> optionalModuleName of
            Nothing         -> pretty i
            Just moduleName -> pretty moduleName <> "." <> pretty i

