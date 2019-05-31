module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)

import Test.Language.JS.AST.Pretty (suite) as Pretty

main :: Effect Unit
main = runTest $ do
  Pretty.suite


