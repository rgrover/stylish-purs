{-# LANGUAGE QuasiQuotes #-}
module Main where

import           Quoter

main :: IO ()
main = do
    let out = [fmt|
module Test.Main where

diagonal :: Number → Number → Number
diagonal w h = sqrt (w * w + h * h + 2.0) -- testing comments

main :: Effect Unit
main = logShow $ diagonal 3.0 4.0
     |]
    putStrLn out
