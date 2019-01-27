{-# LANGUAGE OverloadedStrings #-}
module Format.Formatter (format, formatModule) where

import           Language.PureScript.AST.Declarations
import           Language.PureScript.Names
import           Language.PureScript.Parser.Declarations (parseModuleFromFile)

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Format.Declaration

import           Data.Validation
import           Format.EitherPrettyOrErrors

import           Data.Text                               (Text,
                                                          intercalate,
                                                          pack,
                                                          unpack)

format :: String -> String
format s =
    case parseModuleFromFile id ("_quoted", pack s) of
        Left e                  -> "failed to parse input"
        Right (_, parsedModule) ->
            let output =
                    case formatModule parsedModule of
                        Success doc  -> doc
                        Failure errs -> vsep (pretty <$> errs)
            in unpack
                . renderStrict
                . layoutPretty defaultLayoutOptions $
                output

formatModule :: Module -> Output ann
formatModule parsedModule = addHeader <$> declarationDocs
  where header          = prettyModuleHeader parsedModule
        ds              = getModuleDeclarations parsedModule
        declarationDocs = (prettyE moduleSpan . ModuleDeclarations) ds
        moduleSpan      = getModuleSourceSpan parsedModule
        addHeader docs  = header <> line <> docs

prettyModuleHeader :: Module -> Doc ann
prettyModuleHeader parsedModule =
    "module" <+> prettyName <+> "where" <> line
  where
    ModuleName nameComponents = getModuleName parsedModule
    prettyName =
        pretty $ intercalate "." (runProperName <$> nameComponents)
