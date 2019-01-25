module Main where

import           Format.Formatter

import           Language.PureScript
import           System.IO.UTF8      (readUTF8File)

import           Data.Text.IO        as T (getContents, putStr,
                                           readFile)
import           System.Environment
import           System.Exit
import qualified System.IO           as IO

main :: IO ()
main = do
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8
    args <- getArgs
    text <- if null args
                then T.getContents
                else T.readFile $ head args
    case parseModuleFromFile id ("_stdin", text) of
        Left error              -> print error >> exitFailure
        Right (_, parsedModule) -> T.putStr $ formatModule parsedModule
