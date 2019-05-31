module Test.Language.JS.AST.Pretty where

import Prelude

import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Language.JS.AST (JS(..))
import Language.JS.Pretty (print1)
import Test.Unit (TestSuite, test)
import Test.Unit (suite) as Test
import Test.Unit.Assert (equal)

suite ∷ TestSuite
suite = Test.suite "Data.List.Pointed" $ do
  let
    obj = JSObjectLiteral
      [ Tuple "string" $ JSStringLiteral "value"
      , Tuple "number" $ JSNumericLiteral 8.0
      , Tuple "boolean" $ JSBooleanLiteral true
      ]
    s = joinWith "\n"
      [ "{"
      , "    string: \"value\","
      , "    number: 8.0,"
      , "    boolean: true"
      , "}"
      ]
  test "plain" do
    equal s (print1 obj)
  -- test "fold" do
  --   equal 8 9
