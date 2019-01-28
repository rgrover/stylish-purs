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
import           Data.Maybe                           (isJust)
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

    prettyE _ decl@(ImportDeclaration sa name typ optional) =
        let span          = fst sa
            prettyName    = pretty name
            qualifiedName = case optional of
                Nothing -> Success emptyDoc
                Just q  -> Success $ "as" <+> pretty q
            rootImport = Success $ "import" <+> prettyName
        in case typ of
                Implicit -> if isJust optional
                                then liftA2 (<+>) rootImport qualifiedName
                                else rootImport
                Explicit drefs ->
                    let refFmts  = traverse (prettyE span) drefs
                        foldrefs = encloseSep lparen
                                              rparen
                                              (comma <> space)
                        formattedRefs = foldrefs <$> refFmts
                        rootAndRefs   = liftA2 (<+>)
                                               rootImport
                                               formattedRefs
                    in if isJust optional
                            then liftA2 (<+>) rootAndRefs qualifiedName
                            else rootAndRefs
                _ -> Failure
                        [ UnhandledError
                            ( span
                            , "unhandled import declaration for "
                                ++ show prettyName
                            )
                        ]

    prettyE _ decl =
        let span = fst $ declSourceAnn decl
        in Failure
            [ UnhandledError ( span
                             , "unhandled declaration " ++ show decl
                             )
            ]

declComments :: Declaration -> Maybe (Doc ann)
declComments decl =
    if null comments
        then Nothing
        else Just $ vsep (pretty <$> comments)
  where (_, comments) = declSourceAnn decl

instance EitherPrettyOrErrors DeclarationRef where
    prettyE _ (ValueRef span i) = prettyE span i
    prettyE _ (TypeRef span pname Nothing) = prettyE span pname
    prettyE _ (TypeRef span pname (Just [])) = prettyE span pname
    prettyE _ ref =
        Failure
            [ UnhandledError ( declRefSourceSpan ref
                            , "unhandled declarationRef " ++ show ref
                            )
            ]

newtype ModuleDeclarations = ModuleDeclarations [Declaration]

instance EitherPrettyOrErrors ModuleDeclarations where
    prettyE :: SourceSpan -> ModuleDeclarations -> Output ann
    prettyE span (ModuleDeclarations decls) = joinDocsForDecls decls
      where
        joinDocsForDecls :: [Declaration] -> Output ann
        joinDocsForDecls [] = Success emptyDoc
        joinDocsForDecls decls =
            snd $ foldl' combine (firstLine, firstDoc) $ tail decls
          where
            firstDecl = head decls
            firstLine = sourceLine firstDecl
            firstDoc = prettyE span firstDecl
            combine :: (Int, Output ann)
                    -> Declaration
                    -> (Int, Output ann)
            combine (prevLine, collectedDocs) decl
                | siblingDecls = ( nextLine
                                 , liftA2 (<>) collectedDocs thisDoc
                                 )
                | adjacentDecls = ( nextLine
                                 , liftA2
                                        (\coll this -> coll <> line <> this)
                                        collectedDocs thisDoc
                                 )
                | otherwise    = ( nextLine
                                 , liftA2
                                       (\coll this -> coll <> line <> line <> this)
                                       collectedDocs thisDoc
                                 )
                where
                    nextLine      = sourceLine decl
                    siblingDecls  = prevLine == nextLine
                    adjacentDecls = (prevLine + 1) == nextLine
                    thisDoc       = prettyE span decl

            sourceLine = sourcePosLine . spanStart . declSourceSpan
