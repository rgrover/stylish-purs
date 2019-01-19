{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
module Format.Expr where

import           Language.PureScript.AST.Binders
import           Language.PureScript.AST.Declarations
import           Language.PureScript.AST.Literals
import           Language.PureScript.Names
import           Language.PureScript.Types

import           Data.Text.Prettyprint.Doc

import           Data.Text                            (Text, unpack)

instance Pretty Expr where
    pretty :: Expr -> Doc ann
    pretty (PositionedValue _ cs e) = pretty e
    pretty (Var _ (Qualified Nothing ident)) =
        (pretty . showIdent) ident
    pretty (App e1 e2) = sep (pretty <$> [e1, e2])
    pretty (Parens e) = "(" <> pretty e <> ")"
    pretty (BinaryNoParens e1 e2 e3) = sep (pretty <$> [e2, e1, e3])
    pretty (Op _ (Qualified Nothing opname)) = pretty op
        where op = runOpName opname
    pretty (Literal _ le) = pretty le
    pretty e = pretty ("unhandled expr: " ++ show e)

instance Pretty a => Pretty (Literal a) where
    pretty :: Literal a -> Doc ann
    pretty (NumericLiteral e) =
        case e of
            Left z  -> pretty z
            Right d -> pretty d
    pretty (StringLiteral str) = (pretty . show) str
    pretty (CharLiteral c) = pretty c
    pretty (BooleanLiteral b) = (pretty . show) b
    pretty (ArrayLiteral as) = list (pretty <$> as)
    pretty e = pretty ("literal" :: String)
