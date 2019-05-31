module Language.JS.AST.Utils where

import Prelude

import Data.Array (elem)
import Data.Foldable (foldMap)
import Data.String (Pattern(..)) as String
import Data.String (codePointAt, split)
import Data.String.Regex (Regex, parseFlags, test)
import Data.String.Regex.Unsafe (unsafeRegex)

-- |
-- Convert an identifier into a valid Javascript identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved javascript identifiers are prefixed with '$$'.
--
--  * Symbols are prefixed with '$' followed by a symbol name or their ordinal value.
--
identToJs :: String -> String
identToJs name | nameIsJsReserved name = "$$" <> name
identToJs name = foldMap identCharToString (split (String.Pattern "") name)

-- |
-- Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
--
identCharToString :: String -> String
identCharToString c | test rxAlphaNum c = c
identCharToString "_" = "_"
identCharToString "." = "$dot"
identCharToString "$" = "$dollar"
identCharToString "~" = "$tilde"
identCharToString "=" = "$eq"
identCharToString "<" = "$less"
identCharToString ">" = "$greater"
identCharToString "!" = "$bang"
identCharToString "#" = "$hash"
identCharToString "%" = "$percent"
identCharToString "^" = "$up"
identCharToString "&" = "$amp"
identCharToString "|" = "$bar"
identCharToString "*" = "$times"
identCharToString "/" = "$div"
identCharToString "+" = "$plus"
identCharToString "-" = "$minus"
identCharToString ":" = "$colon"
identCharToString "\\" = "$bslash"
identCharToString "?" = "$qmark"
identCharToString "@" = "$at"
identCharToString "\'" = "$prime"
identCharToString c = "$" <> show (codePointAt 0 c)

-- |
-- Checks whether an identifier name is reserved in Javascript.
--
nameIsJsReserved :: String -> Boolean
nameIsJsReserved name =
  name `elem` [ "abstract"
              , "arguments"
              , "boolean"
              , "break"
              , "byte"
              , "case"
              , "catch"
              , "char"
              , "class"
              , "const"
              , "continue"
              , "debugger"
              , "default"
              , "delete"
              , "do"
              , "double"
              , "else"
              , "enum"
              , "eval"
              , "export"
              , "extends"
              , "final"
              , "finally"
              , "float"
              , "for"
              , "function"
              , "goto"
              , "if"
              , "implements"
              , "import"
              , "in"
              , "instanceof"
              , "int"
              , "interface"
              , "let"
              , "long"
              , "native"
              , "new"
              , "null"
              , "package"
              , "private"
              , "protected"
              , "public"
              , "return"
              , "short"
              , "static"
              , "super"
              , "switch"
              , "synchronized"
              , "this"
              , "throw"
              , "throws"
              , "transient"
              , "try"
              , "typeof"
              , "var"
              , "void"
              , "volatile"
              , "while"
              , "with"
              , "yield" ]

-- |
-- Test if a string is a valid JS identifier (may return false negatives)
--
isIdent :: String -> Boolean
isIdent = test rxIdent

rxAlphaNum :: Regex
rxAlphaNum = unsafeRegex "[a-z0-9]" (parseFlags "i")

rxIdent :: Regex
rxIdent = unsafeRegex "^[a-z][a-z0-9]*$" (parseFlags "i")
