{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Format.Type where

import           Language.PureScript.Names
import           Language.PureScript.Types

import           Data.Text.Prettyprint.Doc

import           Data.Text                 (Text, unpack)

instance Pretty Type where
    pretty :: Type -> Doc ann
    pretty (TypeConstructor (Qualified Nothing n)) =
        pretty $ runProperName n
    pretty (TypeApp
            (TypeConstructor
                (Qualified (Just (ModuleName [ProperName "Prim"]))
                    (ProperName "Function")))
            (TypeConstructor
                (Qualified Nothing (ProperName n)))) =
                    pretty (unpack n) <+> "->"
    pretty (TypeApp
            t1@(TypeApp _ _)
            (TypeConstructor
                (Qualified Nothing (ProperName n)))) =
                    pretty t1 <+> pretty (unpack n)
    pretty (TypeApp t1@(TypeApp _ _) t2@(TypeApp _ _)) =
        pretty t1 <+> pretty t2
    pretty _ = error "unhandled type"


