{-# LANGUAGE OverloadedStrings #-}
module Format.Formatter (format) where

import           Language.Haskell.TH.Syntax              (liftData)

import           Language.PureScript.AST.Declarations
import           Language.PureScript.Names
import           Language.PureScript.Parser.Declarations (parseModuleFromFile)

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Data.Text                               hiding (head,
                                                          tail)

import           Format.Declaration

format :: String -> String
format s =
  case parseModuleFromFile id ("_quoted", pack s) of
      Left e       -> "failed to parse input"
      Right (_, m) ->
          let header =
                sep (pretty <$> ["module", prettyModuleName m, "where"])
                <> line
              firstDeclaration =
                prettyDeclaration $ head $ tail $ getModuleDeclarations m
              doc = header <> line <> firstDeclaration
          in unpack . renderStrict . layoutPretty defaultLayoutOptions
            $ doc

prettyModuleName :: Module -> Text
prettyModuleName m =
    intercalate (singleton '.') (runProperName <$> nameComponents)
  where
    ModuleName nameComponents = getModuleName m
