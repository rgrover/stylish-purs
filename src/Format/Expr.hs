{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Format.Expr where

import           Language.PureScript.AST.Binders
import           Language.PureScript.AST.Declarations
import           Language.PureScript.AST.Literals
import           Language.PureScript.AST.SourcePos
import           Language.PureScript.Names
import           Language.PureScript.Types

import           Data.Text.Prettyprint.Doc

import           Format.Ident

import           Data.Validation
import           Format.EitherPrettyOrErrors

import           Data.Text                            (Text, unpack)

instance EitherPrettyOrErrors Expr where
    prettyE :: SourceSpan -> Expr -> Output ann
    prettyE _ (PositionedValue span cs e)  =
        prettyE span e
    prettyE _ (Var span qualifiedIdent)    =
        prettyE span qualifiedIdent
    prettyE span (App e1 e2)               =
        sep <$> traverse (prettyE span) [e1, e2]
    prettyE span (Parens e)                =
        (\doc -> "(" <> doc <> ")") <$> prettyE span e
    prettyE span (BinaryNoParens e1 e2 e3) =
        sep <$> traverse (prettyE span) [e2, e1, e3]
    prettyE _ (Op span qualifiedOp)        =
        prettyE span qualifiedOp
    prettyE _ (Literal span le)            =
        prettyE span le
    prettyE span e                         =
        Failure [UnhandledError (span, "unhandled expr " ++ show e)]

instance (Show a, EitherPrettyOrErrors a) =>
         EitherPrettyOrErrors (Literal a) where
    prettyE :: SourceSpan -> Literal a -> Output ann
    prettyE _ (NumericLiteral e) =
        case e of
            Left z  -> Success $ pretty z
            Right d -> Success $ pretty d
    prettyE _ (StringLiteral str)  = (Success . pretty . show) str
    prettyE _ (CharLiteral c)      = Success $ pretty c
    prettyE _ (BooleanLiteral b)   = (Success . pretty . show) b
    prettyE span (ArrayLiteral as) = list <$> traverse (prettyE span) as
    prettyE span l =
        Failure [UnhandledError (span, "unhandled literal" ++ show l)]

instance EitherPrettyOrErrors (OpName a) where
    prettyE _ = Success . pretty . runOpName
