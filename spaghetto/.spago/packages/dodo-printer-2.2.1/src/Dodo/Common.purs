module Dodo.Common where

import Prelude

import Dodo (Doc, break, enclose, encloseEmptyAlt, flexAlt, flexGroup, indent, text)

leadingComma :: forall a. Doc a
leadingComma = flexAlt (text ", ") (break <> text ", ")

trailingComma :: forall a. Doc a
trailingComma = flexAlt (text ", ") (text "," <> break)

pursCurlies :: forall a. Doc a -> Doc a
pursCurlies = flexGroup <<< encloseEmptyAlt open close (text "{}")
  where
  open = text "{ "
  close = flexAlt (text " }") (break <> text "}")

pursSquares :: forall a. Doc a -> Doc a
pursSquares = flexGroup <<< encloseEmptyAlt open close (text "[]")
  where
  open = text "[ "
  close = flexAlt (text " ]") (break <> text "]")

pursParens :: forall a. Doc a -> Doc a
pursParens = flexGroup <<< encloseEmptyAlt open close (text "()")
  where
  open = flexAlt (text "(") (text "( ")
  close = flexAlt (text ")") (break <> text ")")

pursParensExpr :: forall a. Doc a -> Doc a
pursParensExpr = flexGroup <<< enclose open close <<< indent
  where
  open = text "("
  close = text ")"

jsCurlies :: forall a. Doc a -> Doc a
jsCurlies = flexGroup <<< encloseEmptyAlt open close (text "{}") <<< indent
  where
  open = flexAlt (text "{") (text "{" <> break)
  close = flexAlt (text "}") (break <> text "}")

jsSquares :: forall a. Doc a -> Doc a
jsSquares = flexGroup <<< encloseEmptyAlt open close (text "[]") <<< indent
  where
  open = flexAlt (text "[") (text "[" <> break)
  close = flexAlt (text "]") (break <> text "]")

jsParens :: forall a. Doc a -> Doc a
jsParens = flexGroup <<< encloseEmptyAlt open close (text "()") <<< indent
  where
  open = flexAlt (text "(") (text "(" <> break)
  close = flexAlt (text ")") (break <> text ")")
