module Main where

import           Format.Formatter

import           Language.PureScript                   (parseModuleFromFile)
import           System.IO.UTF8                        (readUTF8File)

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.Validation

import           Data.Text.IO                          as T (getContents,
                                                             putStr,
                                                             readFile)
import           System.Environment
import           System.Exit
import qualified System.IO                             as IO

main :: IO ()
main = do
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8

    args <- getArgs
    let source = if null args then "_stdin" else head args
    text <- if null args
                then T.getContents
                else T.readFile source

    case parseModuleFromFile id (source, text) of
        Left parseError -> do
            print parseError
            exitFailure
        Right (_, parsedModule) ->
            case formatModule parsedModule of
                Success doc  -> do
                    T.putStr (renderDoc doc)
                    exitSuccess
                Failure unhandledErrs -> do
                    let errDoc = vsep (pretty <$> unhandledErrs) <> line
                    T.putStr $ renderDoc errDoc
                    exitFailure

renderDoc = renderStrict . layoutPretty defaultLayoutOptions
