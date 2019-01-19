{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Format.Declaration where

import           Language.PureScript.AST.Binders
import           Language.PureScript.AST.Declarations
import           Language.PureScript.Names
import           Language.PureScript.Types

import           Format.Expr

import           Data.Text.Prettyprint.Doc

import           Data.Text                            (Text, unpack)

instance Pretty Declaration where
    pretty :: Declaration -> Doc ann
    pretty (TypeDeclaration d) =
        let (i, t) = unwrapTypeDeclaration d
        in pretty i <+>
           pretty ("::" :: String) <+> pretty t
    pretty (ValueDeclaration d) =
        prettyIdent <+> prettyBinders <+> "=" <+> prettyExpression
      where
        prettyIdent = pretty . showIdent . valdeclIdent $ d
        prettyBinders = sep docs
          where
            bs :: [Binder]
            bs = valdeclBinders d
            idents :: [Ident]
            idents = concat (binderNames <$> bs)
            docs = pretty . showIdent <$> idents
        prettyExpression =
            case guardedExpr of
                GuardedExpr [] expr -> pretty expr
                _                   ->
                    pretty ("unhandled guardedExpr" :: String)
          where
            guardedExprs = valdeclExpression d
            guardedExpr = head guardedExprs
    pretty _ = pretty ("unhandled declaration type" :: String)

instance Pretty Ident where
    pretty :: Ident -> Doc ann
    pretty (Ident t) = pretty t
    pretty _         = error "unhandled ident"

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

