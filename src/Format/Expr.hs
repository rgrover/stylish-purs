{-# LANGUAGE OverloadedStrings #-}
module Format.Expr where

import           Language.PureScript.AST.Binders
import           Language.PureScript.AST.Declarations
import           Language.PureScript.Names
import           Language.PureScript.Types

import           Data.Text.Prettyprint.Doc

import           Data.Text                            (Text, unpack)

prettyExpr :: Expr -> Doc ann
prettyExpr (PositionedValue _ cs e) = prettyExpr e
prettyExpr (Var _ (Qualified Nothing ident)) =
    (pretty . showIdent) ident
prettyExpr (App e1 e2) = sep (prettyExpr <$> [e1, e2])
prettyExpr (Parens e) = "(" <> prettyExpr e <> ")"
prettyExpr (BinaryNoParens e1 e2 e3) = sep (prettyExpr <$> [e2, e1, e3])
prettyExpr (Op _ (Qualified Nothing opname)) = pretty op
    where op = runOpName opname
prettyExpr _ = pretty ("unhandled expr" :: String)
