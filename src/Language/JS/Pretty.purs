-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.JS
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- |
-- Pretty printer for the Javascript AST
--
-----------------------------------------------------------------------------

module Language.JS.Pretty where
--   (
--     -- print
--   ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer, fix)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (modify_)
import Control.Monad.State.Trans (get)
import Data.Maybe (Maybe, fromJust)
import Data.String (joinWith)
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (Tuple(..))
import Language.JS.AST (JS(..))
import Language.JS.AST.Utils (isIdent)
import Partial.Unsafe (unsafePartial)
import Text.Pretty.PatternArrows (OperatorTable(..), buildPrettyPrinter, mkPattern', runPattern)
import Text.Pretty.PatternArrows (Pattern(..)) as PatternArrows

-- |
-- Wrap a string in parentheses
--
parens ∷ String → String
parens s = "(" <> s <> ")"

newtype Indent = Indent Int
derive newtype instance semiringIndent ∷ Semiring Indent
derive newtype instance ringIndent ∷ Ring Indent

-- type Ctx a = StateT Indent Maybe a

-- |
-- Number of characters per identation level
--
blockIndent ∷ Indent
blockIndent = Indent 4

type Pattern a = PatternArrows.Pattern Indent JS a
type PatternM b = (StateT Indent Maybe) b

-- |
-- Pretty print with a new indentation level
--
withIndent ∷ ∀ a. PatternM a → PatternM a
withIndent action = do
  modify_ $ add blockIndent
  result ← action
  modify_ $ flip sub blockIndent
  pure result

-- -- |
-- -- Get the current indentation level
-- --
currentIndent ∷ PatternM String
currentIndent = do
  Indent current ← get
  pure $ replicate current " "
  where
  replicate 0 x = ""
  replicate n x = x <> replicate (n - 1) x


foreign import string ∷ String → String

-- |
-- Generate an indented, pretty-printed string representing a Javascript expression
--
print' ∷ JS → PatternM String
print' js = runPattern matchValue js
  where
  -- matchValue ∷ PatternM String
  matchValue = fix $ \p → buildPrettyPrinter operators (literals <|> (<$>) parens p)
  operators ∷ OperatorTable Indent JS String
  operators = OperatorTable []
--     OperatorTable [ [ Operator (wrap (accessor unit) $ \prop val → val ++ "." ++ prop) ]
--                   , [ Operator (wrap (indexer unit) $ \index val → val ++ "[" ++ index ++ "]") ]
--                   , [ Operator (wrap (app unit) $ \args val → val ++ "(" ++ args ++ ")") ]
--                   , [ Operator (wrap lam $ \(Tuple name args) ret → "function "
--                         ++ fromMaybe "" name
--                         ++ "(" ++ S.joinWith ", " args ++ ") "
--                         ++ ret) ]
--                   , [ binary    LessThan             "<" ]
--                   , [ binary    LessThanOrEqualTo    "<=" ]
--                   , [ binary    GreaterThan          ">" ]
--                   , [ binary    GreaterThanOrEqualTo ">=" ]
--                   , [ Operator (wrap typeOf $ \_ s → "typeof " ++ s) ]
--                   , [ unary     Not                  "!" ]
--                   , [ unary     BitwiseNot           "~" ]
--                   , [ unary     Negate               "-" ]
--                   , [ unary     Positive             "+" ]
--                   , [ binary    Multiply             "*" ]
--                   , [ binary    Divide               "/" ]
--                   , [ binary    Modulus              "%" ]
--                   , [ binary    Add                  "+" ]
--                   , [ binary    Subtract             "-" ]
--                   , [ binary    ShiftLeft            "<<" ]
--                   , [ binary    ShiftRight           ">>" ]
--                   , [ binary    ZeroFillShiftRight   ">>>" ]
--                   , [ binary    EqualTo              "===" ]
--                   , [ binary    NotEqualTo           "!==" ]
--                   , [ binary    BitwiseAnd           "&" ]
--                   , [ binary    BitwiseXor           "^" ]
--                   , [ binary    BitwiseOr            "|" ]
--                   , [ binary    And                  "&&" ]
--                   , [ binary    Or                   "||" ]
--                   , [ Operator (wrap conditional $ \(Tuple th el) cond → cond ++ " ? " ++ print1 th ++ " : " ++ print1 el) ]
--                     ]
-- 

literals ∷ Pattern String
literals = defer $ \_ → mkPattern' match
  where
  match ∷ JS → PatternM String
  match (JSNumericLiteral n) = pure $ show n
  match (JSStringLiteral s) = pure $ string s
  match (JSBooleanLiteral true) = pure "true"
  match (JSBooleanLiteral false) = pure "false"
  match (JSArrayLiteral xs) = joinWith "" <$> sequence
    [ pure "[ "
    , joinWith ", " <$> for xs print'
    , pure " ]"
    ]
  match (JSObjectLiteral []) = pure "{}"
  match (JSObjectLiteral ps) = joinWith "" <$> sequence
    [ pure "{\n"
    , withIndent $ do
        jss ← for ps $ \(Tuple key value) → (<$>) (\s → (objectPropertyToString key <> ": ") <> s) <<< print' $ value
        indentString ← currentIndent
        pure $ joinWith ",\n" $ map (\s → indentString <> s) jss
    , pure "\n"
    , currentIndent
    , pure "}"
    ]
    where
    objectPropertyToString ∷ String → String
    objectPropertyToString s | isIdent s = s
    objectPropertyToString s = show s
  match _ = pure "UNMATCHED"
--   match (JSBlock sts) = S.joinWith "" <$> sequence
--     [ return "{\n"
--     , withIndent $ prettyStatements sts
--     , return "\n"
--     , currentIndent
--     , return "}"
--     ]
--   match (JSVar ident) = return ident
--   match (JSVariableIntroduction ident value) = S.joinWith "" <$> sequence
--     [ return "var "
--     , return ident
--     , maybe (return "") ((<$>) (\s → " = " ++ s) <<< print') value
--     ]
--   match (JSAssignment target value) = S.joinWith "" <$> sequence
--     [ print' target
--     , return " = "
--     , print' value
--     ]
--   match (JSWhile cond sts) = S.joinWith "" <$> sequence
--     [ return "while ("
--     , print' cond
--     , return ") "
--     , print' sts
--     ]
--   match (JSFor ident start end sts) = S.joinWith "" <$> sequence
--     [ return $ "for (var " ++ ident ++ " = "
--     , print' start
--     , return $ "; " ++ ident ++ " < "
--     , print' end
--     , return $ "; " ++ ident ++ "++) "
--     , print' sts
--     ]
--   match (JSForIn ident obj sts) = S.joinWith "" <$> sequence
--     [ return $ "for (var " ++ ident ++ " in "
--     , print' obj
--     , return ") "
--     , print' sts
--     ]
--   match (JSIfElse cond thens elses) = S.joinWith "" <$> sequence
--     [ return "if ("
--     , print' cond
--     , return ") "
--     , print' thens
--     , maybe (return "") ((<$>) (\s → " else " ++ s) <<< print') elses
--     ]
--   match (JSReturn value) = S.joinWith "" <$> sequence
--     [ return "return "
--     , print' value
--     ]
--   match (JSThrow value) = S.joinWith "" <$> sequence
--     [ return "throw "
--     , print' value
--     ]
--   match (JSBreak lbl) = return $ "break " ++ lbl
--   match (JSContinue lbl) = return $ "continue " ++ lbl
--   match (JSLabel lbl js) = S.joinWith "" <$> sequence
--     [ return $ lbl ++ ": "
--     , print' js
--     ]
--   match (JSRaw js) = return js
--   match _ = lift Nothing
-- 
-- conditional ∷ Pattern PrinterState JS (Tuple (Tuple JS JS) JS)
-- conditional = mkPattern match
--   where
--   match (JSConditional cond th el) = Just (Tuple (Tuple th el) cond)
--   match _ = Nothing
-- 
-- accessor ∷ Unit → Pattern PrinterState JS (Tuple String JS)
-- accessor _ = mkPattern match
--   where
--   match (JSAccessor prop val) = Just (Tuple prop val)
--   match _ = Nothing
-- 
-- indexer ∷ Unit → Pattern PrinterState JS (Tuple String JS)
-- indexer _ = mkPattern' match
--   where
--   match (JSIndexer index val) = Tuple <$> print' index <*> pure val
--   match _ = lift Nothing
-- 
-- lam ∷ Pattern PrinterState JS (Tuple (Tuple (Maybe String) [String]) JS)
-- lam = mkPattern match
--   where
--   match (JSFunction name args ret) = Just (Tuple (Tuple name args) ret)
--   match _ = Nothing
-- 
-- app ∷ Unit → Pattern PrinterState JS (Tuple String JS)
-- app _ = mkPattern' match
--   where
--   match (JSApp val args) = do
--     jss ← traverse print' args
--     return (Tuple (S.joinWith ", " jss) val)
--   match _ = lift Nothing
-- 
-- typeOf ∷ Pattern PrinterState JS (Tuple Unit JS)
-- typeOf = mkPattern match
--   where
--   match (JSTypeOf val) = Just (Tuple unit val)
--   match _ = Nothing
-- 
-- unary ∷ UnaryOperator → String → Operator PrinterState JS String
-- unary op str = Operator (wrap match (++))
--   where
--   match ∷ Pattern PrinterState JS (Tuple String JS)
--   match = mkPattern match'
--     where
--     match' (JSUnary op' val) | op' == op = Just (Tuple str val)
--     match' _ = Nothing
-- 
-- binary ∷ BinaryOperator → String → Operator PrinterState JS String
-- binary op str = Operator (assocR match (\v1 v2 → v1 ++ " " ++ str ++ " " ++ v2))
--   where
--   match ∷ Pattern PrinterState JS (Tuple JS JS)
--   match = mkPattern match'
--     where
--     match' (JSBinary op' v1 v2) | op' == op = Just (Tuple v1 v2)
--     match' _ = Nothing
-- 
-- prettyStatements ∷ [JS] → StateT PrinterState Maybe String
-- prettyStatements sts = do
--   jss ← for sts print'
--   indentString ← currentIndent
--   return $ S.joinWith "\n" $ map (\s → indentString ++ s ++ ";") jss

-- -- |
-- -- Generate a pretty-printed string representing a Javascript expression
-- --
print1 ∷ JS → String
print1 js = unsafePartial $ fromJust $ evalStateT (print' js) (Indent 0)

-- -- |
-- -- Generate a pretty-printed string representing a collection of Javascript expressions at the same indentation level
-- --
-- print ∷ [JS] → String
-- print jss = runPretty (flip evalStateT (PrinterState { indent: 0 }) <<< prettyStatements) jss
