{-# LANGUAGE OverloadedStrings #-}
module Format.Declaration where

import           Language.PureScript.AST.Declarations
import           Language.PureScript.Names
import           Language.PureScript.Types

import           Data.Text.Prettyprint.Doc

import           Data.Text                            (unpack)

prettyDeclaration :: Declaration -> Doc ann
prettyDeclaration (TypeDeclaration d) =
    let (i, t) = unwrapTypeDeclaration d
    in prettyTypeDeclIdent i <+>
       pretty ("::" :: String) <+>
       prettyType t <>
       line
prettyDeclaration (ValueDeclaration d) = pretty ("value declaration" :: String)
prettyDeclaration _ = pretty ("unhandled declaration type" :: String)

prettyTypeDeclIdent :: Ident -> Doc ann
prettyTypeDeclIdent (Ident t) = pretty t
prettyTypeDeclIdent _         = error "unhandled ident"

prettyType :: Type -> Doc ann
prettyType (TypeConstructor (Qualified Nothing n)) = pretty $ runProperName n
prettyType (TypeApp
        (TypeConstructor
            (Qualified (Just (ModuleName [ProperName "Prim"]))
                (ProperName "Function")))
        (TypeConstructor
            (Qualified Nothing (ProperName n)))) =
                pretty (unpack n) <+> "->"
prettyType (TypeApp
        t1@(TypeApp _ _)
        (TypeConstructor
            (Qualified Nothing (ProperName n)))) =
                prettyType t1 <+> pretty (unpack n)
prettyType (TypeApp t1@(TypeApp _ _) t2@(TypeApp _ _)) =
    prettyType t1 <+> prettyType t2
prettyType _ = error "unhandled type"

