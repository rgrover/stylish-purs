{-# LANGUAGE OverloadedStrings #-}
module Formatter (fmt, format) where

import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax              (liftData)

import           Language.PureScript.AST.Declarations
import           Language.PureScript.AST.SourcePos
import           Language.PureScript.Names
import           Language.PureScript.Parser.Declarations (parseModuleFromFile,
                                                          toPositionedError)
import           Language.PureScript.Types

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.Text.Prettyprint.Doc.Util

import           Data.Text                               hiding (head)

fmt :: QuasiQuoter
fmt = QuasiQuoter {
      quoteExp  = liftData . format
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

format :: String -> String
format s =
  case parseModuleFromFile id ("_quoted", pack s) of
      Left e            -> "failed to parse input"
      Right (_, m) ->
          let header = sep $ pretty <$> ["module", prettyModuleName m, "where"]
              firstDeclaration = prettyDeclaration $ head $ getModuleDeclarations m
              doc = header <> line <> firstDeclaration
          in unpack . renderStrict . layoutPretty defaultLayoutOptions $ doc
          --P.putDocW 80 $ emitDeclaration $ head $ getModuleDeclarations m

prettyModuleName :: Module -> Text
prettyModuleName m =
    intercalate (singleton '.') (runProperName <$> nameComponents)
  where
    ModuleName nameComponents = getModuleName m

prettyDeclaration :: Declaration -> Doc ann
prettyDeclaration (TypeDeclaration d) =
    prettyTypeDeclIdent (tydeclIdent d) <+> pretty ("::" :: String) <+>
    prettyType (tydeclType d) <>
    line
prettyDeclaration _ = pretty ("unhandled declaration type" :: String)

prettyTypeDeclIdent :: Ident -> Doc ann
prettyTypeDeclIdent (Ident t) = pretty t
prettyTypeDeclIdent _         = error "unhandled ident"

prettyType :: Type -> Doc ann
prettyType (TypeConstructor (Qualified Nothing n)) = pretty $ runProperName n
prettyType (TypeApp
        (TypeConstructor
            (Qualified (Just (ModuleName [ProperName "Prim"]))
                (ProperName "Function")))
        (TypeConstructor
            (Qualified Nothing (ProperName n)))) =
                pretty (unpack n) <+> "->"
prettyType (TypeApp
        t1@(TypeApp _ _)
        (TypeConstructor
            (Qualified Nothing (ProperName n)))) =
                prettyType t1 <+> pretty (unpack n)
prettyType (TypeApp t1@(TypeApp _ _) t2@(TypeApp _ _)) =
    prettyType t1 <+> prettyType t2
prettyType _ = error "unhandled type"
