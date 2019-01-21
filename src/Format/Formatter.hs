{-# LANGUAGE OverloadedStrings #-}
module Format.Formatter (format, formatModule) where

import           Language.PureScript.AST.Declarations
import           Language.PureScript.Names
import           Language.PureScript.Parser.Declarations (parseModuleFromFile)

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Format.Declaration

import           Data.Text                               (Text,
                                                          intercalate,
                                                          pack,
                                                          unpack)

format :: String -> String
format s =
  case parseModuleFromFile id ("_quoted", pack s) of
      Left e                  -> "failed to parse input"
      Right (_, parsedModule) -> unpack $ formatModule parsedModule

formatModule :: Module -> Text
formatModule parsedModule =
    let header          = prettyModuleHeader parsedModule
        decls           = getModuleDeclarations parsedModule
        declarationDocs = (pretty . ModuleDeclarations) decls
        doc             = if null decls
            then header
            else header <> line <> declarationDocs
    in renderStrict . layoutPretty defaultLayoutOptions $ doc

prettyModuleHeader :: Module -> Doc ann
prettyModuleHeader parsedModule =
    "module" <+> prettyName <+> "where" <> line
  where
    ModuleName nameComponents = getModuleName parsedModule
    prettyName = pretty $
        intercalate "." (runProperName <$> nameComponents)
