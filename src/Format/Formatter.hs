{-# LANGUAGE OverloadedStrings #-}
module Format.Formatter (format) where

import           Language.PureScript.AST.Declarations
import           Language.PureScript.Names
import           Language.PureScript.Parser.Declarations (parseModuleFromFile)

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Data.Text                               (intercalate,
                                                          pack,
                                                          unpack)

import           Format.Declaration

format :: String -> String
format s =
  case parseModuleFromFile id ("_quoted", pack s) of
      Left e       -> "failed to parse input"
      Right (_, m) ->
          let header          = prettyModuleHeader m
              decls           = getModuleDeclarations m
              declarationDocs = (pretty . ModuleDeclarations) decls
              doc             = if null decls
                then header
                else header <> line <> declarationDocs
          in unpack . renderStrict . layoutPretty defaultLayoutOptions $ doc

prettyModuleHeader :: Module -> Doc ann
prettyModuleHeader m =
    "module" <+> prettyName <+> "where" <> line
  where
    ModuleName nameComponents = getModuleName m
    prettyName = pretty $
        intercalate "." (runProperName <$> nameComponents)
