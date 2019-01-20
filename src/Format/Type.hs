{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Format.Type where

import           Language.PureScript.Names
import           Language.PureScript.Types

import           Data.Text.Prettyprint.Doc

import           Format.Ident

import           Data.Text                 (Text, unpack)

instance Pretty Type where
    pretty :: Type -> Doc ann
    pretty (TypeConstructor (Qualified Nothing n)) =
        pretty $ runProperName n
    pretty (TypeApp
            (TypeConstructor
                (Qualified (Just (ModuleName [ProperName "Prim"]))
                    (ProperName "Function")))
            (TypeConstructor qualifiedName)) =
        pretty qualifiedName <+> "->"
    pretty (TypeApp
               (TypeConstructor qualifiedName1)
               (TypeConstructor qualifiedName2)) =
        pretty qualifiedName1 <+> pretty qualifiedName2
    pretty (TypeApp t1@(TypeApp _ _) (TypeConstructor qualifiedName)) =
        pretty t1 <+> pretty qualifiedName
    pretty (TypeApp t1@(TypeApp _ _) t2@(TypeApp _ _)) =
        pretty t1 <+> pretty t2
    pretty t = error ("unhandled type: " ++ show t)


