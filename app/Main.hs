{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Language.PureScript.AST.Declarations    (getModuleName)
import           Language.PureScript.Names               (ModuleName (..),
                                                          ProperName (..))
import           Language.PureScript.Parser.Declarations (parseModuleFromFile,
                                                          toPositionedError)
import           System.IO.UTF8                          (readUTF8FileT)

import           Data.Text                               (intercalate,
                                                          singleton)
import           System.Exit

path = "/home/rohit/swim/Purescript/my-project/src/Main.purs"

main :: IO ()
main = do
    t <- readUTF8FileT path
    case parseModuleFromFile id (path, t) of
        Left e       -> die $ show e
        Right (_, m) -> print m >> exitSuccess
        --Right (_, m) -> do
            --let ModuleName moduleNames = getModuleName m
                --name =
                    --intercalate
                        --(singleton '.')
                        --(runProperName <$> moduleNames)
            --print name >> exitSuccess
