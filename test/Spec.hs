{-# LANGUAGE QuasiQuotes #-}
import           Test.Tasty
import           Test.Tasty.HUnit

import           Quoter
import           Text.Heredoc

main :: IO ()
main = defaultMain $
    testGroup "format"
        [ testCase "module header only" $
            [fmt|
module Main where

             |] @?= [here|module Main where
|]
        , testCase "qualified module header only" $
            [fmt| module Test.AnotherLevel.Main where |] @?=
            [here|module Test.AnotherLevel.Main where
|]
        , testCase "single declaration" $
            [fmt|
module Mod where


diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h + 2.0)
             |] @?=
             [here|module Mod where

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h + 2.0)
|]
        , testCase "few simple declarations" $
            [fmt|
module Mod where

  -- | diagonal header
diagonal :: Int -> Number -> Effect Unit
diagonal w h = sqrt (w * w + h * h + 2.0)

-- testing comments
main :: Effect Unit
main = logShow $ diagonal 3.0 4.0
             |] @?=
             [here|module Mod where

-- | diagonal header
diagonal :: Int -> Number -> Effect Unit
diagonal w h = sqrt (w * w + h * h + 2.0)

-- testing comments
main :: Effect Unit
main  = logShow $ diagonal 3.0 4.0
|]
        ]
