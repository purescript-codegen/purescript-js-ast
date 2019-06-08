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

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer, fix)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (modify_)
import Control.Monad.State.Trans (get)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.NonEmpty (singleton)
import Data.String (joinWith)
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (Tuple(..))
import Language.JS.AST (BinaryOperator(..), JS(..), UnaryOperator(..))
import Language.JS.AST.Utils (isIdent)
import Partial.Unsafe (unsafePartial)
import Text.Pretty.PatternArrows (Operator(..), OperatorTable(..), assocR, buildPrettyPrinter, mkPattern, mkPattern', runPattern, wrap)
import Text.Pretty.PatternArrows (Pattern) as PatternArrows

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
  operators = OperatorTable
    [ singleton $ Operator (wrap (accessor unit) $ \prop val → val <> "." <> prop)
    , singleton $ Operator (wrap (indexer unit) $ \index val → val <> "[" <> index <> "]")
    , singleton $ Operator (wrap app $ \args val → val <> "(" <> args <> ")")
    , singleton $ Operator (wrap lam $ \(Tuple name args) ret → "function "
        <> fromMaybe "" name
        <> "(" <> joinWith ", " args <> ") "
        <> ret)
    , singleton $ binary LessThan "<"
    , singleton $ binary LessThanOrEqualTo "<="
    , singleton $ binary GreaterThan ">"
    , singleton $ binary GreaterThanOrEqualTo ">="
    , singleton $ Operator (wrap typeOf $ \_ s → "typeof " <> s)
    , singleton $ unary Not "!"
    , singleton $ unary BitwiseNot "~"
    , singleton $ unary Negate "-"
    , singleton $ unary Positive "+"
    , singleton $ binary Multiply "*"
    , singleton $ binary Divide "/"
    , singleton $ binary Modulus "%"
    , singleton $ binary Add "+"
    , singleton $ binary Subtract "-"
    , singleton $ binary ShiftLeft "<<"
    , singleton $ binary ShiftRight ">>"
    , singleton $ binary ZeroFillShiftRight ">>>"
    , singleton $ binary EqualTo "==="
    , singleton $ binary NotEqualTo "!=="
    , singleton $ binary BitwiseAnd "&"
    , singleton $ binary BitwiseXor "^"
    , singleton $ binary BitwiseOr "|"
    , singleton $ binary And "&&"
    , singleton $ binary Or "||"
    , singleton $ Operator (wrap conditional $ \(Tuple th el) cond → cond <> " ? " <> print1 th <> " : " <> print1 el)
    ]

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
  match (JSBlock sts) = joinWith "" <$> sequence
    [ pure "{\n"
    , withIndent $ prettyStatements sts
    , pure "\n"
    , currentIndent
    , pure "}"
    ]
  match (JSVar ident) = pure ident
  match (JSVariableIntroduction ident value) = joinWith "" <$> sequence
    [ pure "var "
    , pure ident
    , maybe (pure "") ((<$>) (\s → " = " <> s) <<< print') value
    ]
  match (JSAssignment target value) = joinWith "" <$> sequence
    [ print' target
    , pure " = "
    , print' value
    ]
  match (JSWhile cond sts) = joinWith "" <$> sequence
    [ pure "while ("
    , print' cond
    , pure ") "
    , print' sts
    ]
  match (JSFor ident start end sts) = joinWith "" <$> sequence
    [ pure $ "for (var " <> ident <> " = "
    , print' start
    , pure $ "; " <> ident <> " < "
    , print' end
    , pure $ "; " <> ident <> "<>) "
    , print' sts
    ]
  match (JSForIn ident obj sts) = joinWith "" <$> sequence
    [ pure $ "for (var " <> ident <> " in "
    , print' obj
    , pure ") "
    , print' sts
    ]
  match (JSIfElse cond thens elses) = joinWith "" <$> sequence
    [ pure "if ("
    , print' cond
    , pure ") "
    , print' thens
    , maybe (pure "") ((<$>) (\s → " else " <> s) <<< print') elses
    ]
  match (JSReturn value) = joinWith "" <$> sequence
    [ pure "return "
    , print' value
    ]
  match (JSThrow value) = joinWith "" <$> sequence
    [ pure "throw "
    , print' value
    ]
  match (JSBreak lbl) = pure $ "break " <> lbl
  match (JSContinue lbl) = pure $ "continue " <> lbl
  match (JSLabel lbl js) = joinWith "" <$> sequence
    [ pure $ lbl <> ": "
    , print' js
    ]
  match _ = lift Nothing

conditional ∷ Pattern (Tuple (Tuple JS JS) JS)
conditional = mkPattern match
  where
  match (JSConditional cond th el) = Just (Tuple (Tuple th el) cond)
  match _ = Nothing

accessor ∷ Unit → Pattern (Tuple String JS)
accessor _ = mkPattern match
  where
  match (JSAccessor prop val) = Just (Tuple prop val)
  match _ = Nothing

indexer ∷ Unit → Pattern (Tuple String JS)
indexer _ = mkPattern' match
  where
  match (JSIndexer index val) = Tuple <$> print' index <*> pure val
  match _ = lift Nothing

lam ∷ Pattern (Tuple (Tuple (Maybe String) (Array String)) JS)
lam = mkPattern match
  where
  match (JSFunction name args ret) = Just (Tuple (Tuple name args) ret)
  match _ = Nothing

app ∷ Pattern (Tuple String JS)
app = defer \_ → mkPattern' match
  where
  match (JSApp val args) = do
    jss ← traverse print' args
    pure (Tuple (joinWith ", " jss) val)
  match _ = lift Nothing

typeOf ∷ Pattern (Tuple Unit JS)
typeOf = mkPattern match
  where
  match (JSTypeOf val) = Just (Tuple unit val)
  match _ = Nothing

unary ∷ UnaryOperator → String → Operator Indent JS String
unary op str = Operator (wrap match (<>))
  where
  match ∷ Pattern (Tuple String JS)
  match = mkPattern match'
    where
    match' (JSUnary op' val) | op' == op = Just (Tuple str val)
    match' _ = Nothing

binary ∷ BinaryOperator → String → Operator Indent JS String
binary op str = Operator (assocR match (\v1 v2 → v1 <> " " <> str <> " " <> v2))
  where
  match ∷ Pattern (Tuple JS JS)
  match = mkPattern match'
    where
    match' (JSBinary op' v1 v2) | op' == op = Just (Tuple v1 v2)
    match' _ = Nothing

prettyStatements ∷ Array JS → PatternM String
prettyStatements sts = do
  jss ← for sts print'
  indentString ← currentIndent
  pure $ joinWith "\n" $ map (\s → indentString <> s <> ";") jss

-- |
-- Generate a pretty-printed string representing a Javascript expression
--
print1 ∷ JS → String
print1 js = unsafePartial $ fromJust $ evalStateT (print' js) (Indent 0)

-- |
-- Generate a pretty-printed string representing a collection of Javascript expressions at the same indentation level
--
print ∷ Array JS → String
print jss = unsafePartial $ fromJust $ evalStateT (prettyStatements jss) (Indent 0)
