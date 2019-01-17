{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Language.PureScript.AST.Declarations
import           Language.PureScript.AST.SourcePos
import           Language.PureScript.Names
import           Language.PureScript.Parser.Declarations (parseModuleFromFile,
                                                          toPositionedError)
import           Language.PureScript.Types

import           System.IO.UTF8                          (readUTF8FileT)

import           Data.Text                               (intercalate,
                                                          singleton,
                                                          unpack)

import qualified Data.Text.Prettyprint.Doc               as P
import qualified Data.Text.Prettyprint.Doc.Util          as P

import           System.Exit

path = "/home/rohit/swim/Purescript/my-project/src/Main.purs"

main :: IO ()
main = do
    t <- readUTF8FileT path
    case parseModuleFromFile id (path, t) of
        Left e       -> die $ show e
        --Right (_, m) -> print m >> exitSuccess
        Right (_, m) -> do
            let ModuleName moduleNameComponents = getModuleName m
                fullModuleName =
                    intercalate
                        (singleton '.')
                        (runProperName <$> moduleNameComponents)
            print fullModuleName
            P.putDocW 80 $ emitDeclaration $ head $ getModuleDeclarations m
            exitSuccess

emitDeclaration :: Declaration -> P.Doc ann
emitDeclaration (TypeDeclaration d) =
    emitTypeDeclIdent (tydeclIdent d) P.<+> P.pretty ("::" :: String) P.<+>
    emitType (tydeclType d) <>
    P.line
emitDeclaration _ = P.pretty ("unhandled declaration type" :: String)

emitTypeDeclIdent :: Ident -> P.Doc ann
emitTypeDeclIdent (Ident t) = P.pretty t
emitTypeDeclIdent _         = error "unhandled ident"

emitType :: Type -> P.Doc ann
emitType (TypeConstructor (Qualified Nothing n)) = P.pretty $ runProperName n
emitType (TypeApp
            (TypeConstructor
                (Qualified (Just (ModuleName [ProperName "Prim"]))
                    (ProperName "Function")))
            (TypeConstructor
                (Qualified Nothing (ProperName n)))) =
                    P.pretty (unpack n) P.<+> "->"
emitType (TypeApp
            t1@(TypeApp _ _)
            (TypeConstructor
                (Qualified Nothing (ProperName n)))) =
                    emitType t1 P.<+> P.pretty (unpack n)
emitType (TypeApp t1@(TypeApp _ _) t2@(TypeApp _ _)) =
    emitType t1 P.<+> emitType t2
emitType _ = error "unhandled type"

    {-
       {- rohit
           -}
       module Test.Main where


       diagonal :: Number → Number → Number
       diagonal w h = sqrt (w * w + h * h) -- testing comments

       main :: Effect Unit
       main = logShow $ diagonal 3.0 4.0

       -}

    {-
ValueDeclaration
    (ValueDeclarationData
        {valdeclSourceAnn =
            (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 36}},[]),
            valdeclIdent = Ident "diagonal",
            valdeclName = Public,
            valdeclBinders =
                [PositionedBinder (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 10}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 11}}) [] (VarBinder (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 10}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 11}}) (Ident "w")),PositionedBinder (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 12}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 13}}) [] (VarBinder (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 12}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 13}}) (Ident "h"))],
                valdeclExpression =
                    [ GuardedExpr []
                        (PositionedValue
                            (SourceSpan
                                {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 16}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 36}})
                                []
                                (App
                                (PositionedValue (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 16}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 20}}) [] (Var (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 16}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 20}}) (Qualified Nothing (Ident "sqrt"))))
                                (PositionedValue (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 21}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 36}}) [] (Parens (BinaryNoParens (Op (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 24}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 25}}) (Qualified Nothing (OpName {runOpName = "*"}))) (PositionedValue (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 22}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 23}}) [] (Var (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 22}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 23}}) (Qualified Nothing (Ident "w")))) (BinaryNoParens (Op (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 28}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 29}}) (Qualified Nothing (OpName {runOpName = "+"}))) (PositionedValue (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 26}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 27}}) [] (Var (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 26}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 27}}) (Qualified Nothing (Ident "w")))) (BinaryNoParens (Op (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 32}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 33}}) (Qualified Nothing (OpName {runOpName = "*"}))) (PositionedValue (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 30}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 31}}) [] (Var (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 30}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 31}}) (Qualified Nothing (Ident "h")))) (PositionedValue (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 34}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 35}}) [] (Var (SourceSpan {spanName = "/home/rohit/swim/Purescript/my-project/src/Main.purs", spanStart = SourcePos {sourcePosLine = 7, sourcePosColumn = 34}, spanEnd = SourcePos {sourcePosLine = 7, sourcePosColumn = 35}}) (Qualified Nothing (Ident "h")))))))))
                                ))
                    ]
        })
    -}
