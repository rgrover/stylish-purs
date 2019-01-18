{-# LANGUAGE QuasiQuotes #-}
module Main where

import           Formatter

main :: IO ()
main = do
    let out = [fmt|
module Test.Main where

diagonal :: Number → Number → Number
diagonal w h = sqrt (w * w + h * h) -- testing comments

main :: Effect Unit
main = logShow $ diagonal 3.0 4.0
     |]
    putStrLn out

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
