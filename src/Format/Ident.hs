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

instance Pretty ModuleName where
    pretty = pretty . runModuleName

instance EitherPrettyOrErrors a =>
         EitherPrettyOrErrors (Qualified a) where
    prettyE :: SourceSpan -> Qualified a -> Output ann
    prettyE span (Qualified optionalModuleName ident) =
        case pretty <$> optionalModuleName of
            Nothing         -> prettyE span ident
            Just doc -> let prefix = doc <> "."
                        in (prefix <>) <$> prettyE span ident
