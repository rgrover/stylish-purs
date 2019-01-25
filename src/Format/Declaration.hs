{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Format.Declaration where

import           Language.PureScript.AST.Binders
import           Language.PureScript.AST.Declarations
import           Language.PureScript.AST.SourcePos
import           Language.PureScript.Names
import           Language.PureScript.Types

import           Format.Comment
import           Format.Expr
import           Format.Ident
import           Format.Type
import           Format.UnhandledError

import           Data.Text.Prettyprint.Doc

import           Data.Text                            (Text, unpack)

instance Pretty Declaration where
    pretty :: Declaration -> Doc ann
    pretty decl@(TypeDeclaration d) =
        let (i, t)        = unwrapTypeDeclaration d
            doc = pretty i <+> "::" <+> pretty t
        in case declComments decl of
            Nothing    -> doc
            Just cDocs -> cDocs <> line <> doc
    pretty decl@(ValueDeclaration d) =
        case declComments decl of
            Nothing    -> doc
            Just cDocs -> cDocs <> line <> doc
      where
        prettyIdent = pretty . valdeclIdent $ d
        prettyBinders = sep $ pretty <$> idents
          where
            bs :: [Binder]
            bs = valdeclBinders d
            idents :: [Ident]
            idents = concat (binderNames <$> bs)
        prettyExpression =
            case guardedExpr of
                GuardedExpr [] expr -> pretty expr
                _                   ->
                    pretty ("unhandled guardedExpr" :: String)
          where
            guardedExprs = valdeclExpression d
            guardedExpr = head guardedExprs
        doc = prettyIdent <+> prettyBinders <+> "=" <+> prettyExpression
    pretty decl = unhandledError decl

declComments :: Declaration -> Maybe (Doc ann)
declComments decl =
    if null comments
        then Nothing
        else Just $ vsep (pretty <$> comments)
  where (_, comments) = declSourceAnn decl

newtype ModuleDeclarations = ModuleDeclarations [Declaration]

instance Pretty ModuleDeclarations where
    pretty :: ModuleDeclarations -> Doc ann
    pretty (ModuleDeclarations [])    = emptyDoc
    pretty (ModuleDeclarations decls) =
        snd $ foldr combine (lastLine, lastDoc) $ init decls
      where
        lastDecl = last decls
        lastLine = sourceLine lastDecl
        lastDoc  = pretty lastDecl <> line
        combine :: Declaration -> (Int, Doc ann) -> (Int, Doc ann)
        combine decl (prevLine, collectedDocs)
            | adjacentDecls = (newLine, thisDoc <> collectedDocs)
            | otherwise     = (newLine, thisDoc <> line <> collectedDocs)
          where
            newLine       = sourceLine decl
            adjacentDecls = prevLine == (newLine + 1)
            thisDoc       = pretty decl <> line
        sourceLine = sourcePosLine . spanStart . declSourceSpan

instance UnhandledError Declaration where
    unhandledError d =
        pretty ("stylish-purs: " :: String) <>
        pretty (displaySourcePos $ spanStart $ fst $ declSourceAnn d) <+>
        pretty ("::" :: String) <+>
        pretty ("unhandled declaration type" :: String) <+>
        pretty (show d)

