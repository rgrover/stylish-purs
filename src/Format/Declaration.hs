{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Format.Declaration where

import           Language.PureScript.AST.Binders
import           Language.PureScript.AST.Declarations
import           Language.PureScript.AST.SourcePos
import           Language.PureScript.Names
import           Language.PureScript.Types

import           Format.Binder
import           Format.Comment
import           Format.Expr
import           Format.Ident
import           Format.Type

import           Data.Validation
import           Format.EitherPrettyOrErrors

import           Data.Text.Prettyprint.Doc

import           Control.Applicative                  (liftA2, liftA3)
import           Data.List                            (foldl')
import           Data.Text                            (Text, unpack)

instance EitherPrettyOrErrors Declaration where
    prettyE :: SourceSpan -> Declaration -> Output ann
    prettyE _ decl@(TypeDeclaration d) =
        let (i, t) = unwrapTypeDeclaration d
            doc    = liftA2 combine (prettyE span i) (prettyE span t)
        in case declComments decl of
            Nothing    -> doc
            Just cDocs -> (\d -> cDocs <> line <> d) <$> doc
      where
        span              = fst $ tydeclSourceAnn d
        combine ident typ = ident <+> "::" <+> typ

    prettyE _ decl@(ValueDeclaration d) =
        case declComments decl of
            Nothing    -> doc
            Just cDocs -> (\d -> cDocs <> line <> d) <$> doc
      where
        span          = fst $ valdeclSourceAnn d
        prettyIdent   = prettyE span . valdeclIdent $ d
        prettyBinders = sep <$> traverse (prettyE span) (valdeclBinders d)
        prettyExpression =
            case guardedExpr of
                GuardedExpr [] expr -> prettyE span expr
                _                   ->
                    Failure
                        [ UnhandledError
                            ( span , "unhandled guardedExpr ")
                        ]
          where
            guardedExprs = valdeclExpression d
            guardedExpr  = head guardedExprs
        doc = liftA3 (\i binds exp -> i <+> binds <+> "=" <+> exp)
                     prettyIdent
                     prettyBinders
                     prettyExpression

    prettyE span decl =
        Failure [ UnhandledError ( span
                                 , "unhandled declaration " ++ show decl
                                 )
                ]

declComments :: Declaration -> Maybe (Doc ann)
declComments decl =
    if null comments
        then Nothing
        else Just $ vsep (pretty <$> comments)
  where (_, comments) = declSourceAnn decl

newtype ModuleDeclarations = ModuleDeclarations [Declaration]

instance EitherPrettyOrErrors ModuleDeclarations where
    prettyE :: SourceSpan -> ModuleDeclarations -> Output ann
    prettyE span (ModuleDeclarations decls) = joinDocsForDecls decls
      where
        joinDocsForDecls :: [Declaration] -> Output ann
        joinDocsForDecls [] = Success emptyDoc
        joinDocsForDecls decls =
            snd $ foldl' combine (firstLine, firstDoc) decls
          where
            firstDecl = head decls
            firstLine = sourceLine firstDecl
            firstDoc = prettyE span firstDecl
            combine :: (Int, Output ann)
                    -> Declaration
                    -> (Int, Output ann)
            combine (prevLine, collectedDocs) decl
                | siblingDecls = ( newLine
                                 , liftA2 (<>) collectedDocs thisDoc
                                 )
                | otherwise    = ( newLine
                                 , liftA2
                                       (\coll this -> coll <> line <> this)
                                       collectedDocs thisDoc
                                 )
                where
                    newLine      = sourceLine decl
                    siblingDecls = prevLine == newLine
                    thisDoc      = prettyE span decl

            sourceLine = sourcePosLine . spanStart . declSourceSpan
