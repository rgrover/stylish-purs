{-# LANGUAGE InstanceSigs #-}
module Format.Binder where

import           Language.PureScript.AST.Binders

import           Data.Text.Prettyprint.Doc

import           Data.Validation
import           Format.EitherPrettyOrErrors
import           Language.PureScript.AST.SourcePos

instance EitherPrettyOrErrors Binder where
    prettyE :: SourceSpan -> Binder -> Output ann
    prettyE span b =
        Failure [ UnhandledError
                    ( span
                    , "unhandled binder " ++ show b
                    )
                ]
