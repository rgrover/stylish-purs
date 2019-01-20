{-# LANGUAGE OverloadedStrings #-}
module Format.Formatter (format) where

import           Language.PureScript.AST.Declarations
import           Language.PureScript.Names
import           Language.PureScript.Parser.Declarations (parseModuleFromFile)

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Data.Text

import           Format.Declaration

format :: String -> String
format s =
  case parseModuleFromFile id ("_quoted", pack s) of
      Left e       -> "failed to parse input"
      Right (_, m) ->
          let header          = prettyModuleHeader m
              decls           = getModuleDeclarations m
              declarationDocs = (pretty . ModuleDeclarations) decls
              doc             = header <> line <> declarationDocs
          in unpack . renderStrict . layoutPretty defaultLayoutOptions $ doc
          --in unpack . renderStrict . layoutPretty defaultLayoutOptions
             -- $ pretty $ show m

prettyModuleHeader :: Module -> Doc ann
prettyModuleHeader m =
    "module" <+> prettyName <+> "where" <> line
  where
    ModuleName nameComponents = getModuleName m
    prettyName = pretty $
        intercalate (singleton '.') (runProperName <$> nameComponents)
