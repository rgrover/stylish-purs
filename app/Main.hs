module Main where

import           Format.Formatter

import           Language.PureScript
import           System.IO.UTF8      (readUTF8File)

import           Data.Text.IO        as T (getContents, putStr)
import           System.Exit
import qualified System.IO           as IO

main :: IO ()
main = do
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8
    text <- T.getContents
    case parseModuleFromFile id ("_stdin", text) of
        Left error         -> print error >> exitFailure
        Right (_, parsedModule) -> T.putStr $ formatModule parsedModule
