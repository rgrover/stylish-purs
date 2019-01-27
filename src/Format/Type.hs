{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Format.Type where

import           Language.PureScript.AST.SourcePos
import           Language.PureScript.Names
import           Language.PureScript.Types

import           Data.Text.Prettyprint.Doc

import           Format.EitherPrettyOrErrors
import           Format.Ident

import           Data.Validation

import           Control.Applicative
import           Data.Text                         (Text, unpack)

instance EitherPrettyOrErrors Type where
    prettyE :: SourceSpan -> Type -> Output ann
    prettyE _ (TypeConstructor (Qualified Nothing n)) =
        Success $ pretty $ runProperName n
    prettyE span
            (TypeApp
                (TypeConstructor
                    (Qualified
                        (Just (ModuleName [ProperName "Prim"]))
                        (ProperName "Function")))
            (TypeConstructor qualifiedName)) =
        (<+> "->") <$> prettyE span qualifiedName

    prettyE span
            (TypeApp
                (TypeConstructor qualifiedName1)
                (TypeConstructor qualifiedName2)) =
        liftA2 (<+>)
               (prettyE span qualifiedName1)
               (prettyE span qualifiedName2)

    prettyE span
            (TypeApp t1@(TypeApp _ _)
            (TypeConstructor qualifiedName)) =
        liftA2 (<+>) (prettyE span t1) (prettyE span qualifiedName)

    prettyE span (TypeApp t1@(TypeApp _ _) t2@(TypeApp _ _)) =
        liftA2 (<+>) (prettyE span t1) (prettyE span t2)

    prettyE span t =
        Failure [UnhandledError (span, "unhandled type: " ++ show t)]
