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

import           Data.Text.Prettyprint.Doc

import           Data.Text                            (Text, unpack)

instance Pretty Declaration where
    pretty :: Declaration -> Doc ann
    pretty (TypeDeclaration d) =
        let (i, t) = unwrapTypeDeclaration d
        in pretty i <+>
           pretty ("::" :: String) <+> pretty t
    pretty decl@(ValueDeclaration d) =
        cDocs <> line <>
            prettyIdent <+> prettyBinders <+> "=" <+> prettyExpression
      where
        (_, comments) = declSourceAnn decl
        cDocs = vsep (pretty <$> comments)
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

newtype ModuleDeclarations = ModuleDeclarations [Declaration]

instance Pretty ModuleDeclarations where
    pretty :: ModuleDeclarations -> Doc ann
    pretty (ModuleDeclarations [])    = emptyDoc
    pretty (ModuleDeclarations decls) =
        snd $ foldr combine (lastLine, lastDoc) $ init decls
      where
        lastDecl = last decls
        lastLine = sourceLine lastDecl
        lastDoc  = pretty lastDecl
        combine :: Declaration -> (Int, Doc ann) -> (Int, Doc ann)
        combine decl (prevLine, collectedDocs)
            | adjacentDecls = (newLine, thisDoc <> collectedDocs)
            | otherwise     = (newLine, thisDoc <> line <> collectedDocs)
          where
            newLine       = sourceLine decl
            adjacentDecls = prevLine == (newLine + 1)
            thisDoc       = pretty decl <> line
        sourceLine = sourcePosLine . spanStart . declSourceSpan


