{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Format.Ident where

import           Language.PureScript.AST.SourcePos
import           Language.PureScript.Names

import           Data.Validation
import           Format.EitherPrettyOrErrors

import           Data.Text.Prettyprint.Doc

instance EitherPrettyOrErrors Ident where
    prettyE :: SourceSpan -> Ident -> Output ann
    prettyE _ (Ident t) = Success $ pretty t
    prettyE span i      =
        Failure [UnhandledError (span, "unhandled ident " ++ show i)]

instance EitherPrettyOrErrors (ProperName a) where
    prettyE _ = Success . pretty . runProperName

instance EitherPrettyOrErrors a =>
         EitherPrettyOrErrors (Qualified a) where
    prettyE :: SourceSpan -> Qualified a -> Output ann
    prettyE span (Qualified optionalModuleName i) =
        case runModuleName <$> optionalModuleName of
            Nothing         -> prettyE span i
            Just moduleName -> let m = pretty moduleName <> "."
                               in (m <>) <$> prettyE span i
