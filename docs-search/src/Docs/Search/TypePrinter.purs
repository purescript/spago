module Docs.Search.TypePrinter where

import Prelude

import Docs.Search.Extra ((>#>))
import Docs.Search.Terminal (cyan)
import Docs.Search.TypeDecoder (Constraint(..), FunDep(..), FunDeps(..), Kind(..), QualifiedName(..), Type(..), TypeArgument(..), joinForAlls, joinRows)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.List as List


-- | A pretty-printer for types, for TTY with colors.
showType :: Type -> String
showType = case _ of
  TypeVar str -> str
  TypeLevelString str -> "\"" <> str <> "\"" -- TODO: add escaping
  TypeWildcard -> "_"
  TypeConstructor qname -> showQualifiedName qname
  TypeOp qname -> showQualifiedName qname

  TypeApp (TypeApp (TypeConstructor
                    (QualifiedName { moduleNameParts: [ "Prim" ]
                                   , name: "Function" }))
                   t1)
          t2 ->
    showType t1 <> syntax " -> " <> showType t2

  TypeApp (TypeConstructor (QualifiedName { moduleNameParts: [ "Prim" ]
                                          , name: "Record" }))
          row ->
    showRow false row

  TypeApp t1 t2 ->
    showType t1 <> " " <> showType t2

  ty@(ForAll _ _ _) ->
    showForAll ty

  ConstrainedType cnstr ty ->
    showConstraint cnstr <>
    syntax " => " <>
    showType ty

  ty@REmpty -> showRow true ty
  ty@(RCons _ _ _) -> showRow true ty

  BinaryNoParensType op t1 t2 ->
    showType t1 <>
    space <>
    showType op <>
    space <>
    showType t2

  ParensInType ty ->
    "(" <>
    showType ty <>
    ")"


showTypeArgument :: TypeArgument -> String
showTypeArgument (TypeArgument { name, mbKind }) =
  case mbKind of
    Nothing ->
      name
    Just kind ->
      "(" <>
      name <>
      " :: " <>
      showKind kind <>
      ")"


showFunDeps :: FunDeps -> String
showFunDeps (FunDeps []) = ""
showFunDeps (FunDeps deps) =
  append (syntax " | ") $
  Array.intercalate (syntax ", ") $
  deps <#> renderFunDep
  where
    renderFunDep (FunDep { lhs, rhs }) =
      Array.intercalate space lhs <>
      syntax " -> " <>
      Array.intercalate space rhs


showQualifiedName
  :: QualifiedName
  -> String
showQualifiedName (QualifiedName { name })
  = name


showRow
  :: Boolean
  -> Type
  -> String
showRow asRow =
  joinRows >>> \ { rows, ty } ->
  if List.null rows
  then
    if asRow
    then "()"
    else fromMaybe "{}" $ ty <#> \ty' -> "Record " <> showType ty'
  else
    opening <>
    ( Array.intercalate ", " $ Array.fromFoldable $ rows <#>
      \entry ->
      entry.row <> syntax " :: " <> showType entry.ty
    ) <>

    (ty >#> \ty' -> " | " <> showType ty') <>

    closing

    where
      opening = if asRow then "(" else "{ "
      closing = if asRow then ")" else " }"


showForAll
  :: Type
  -> String
showForAll ty =
  keyword "forall" <>

  ( Array.fold $ foralls.binders <#>
    \ { name, mbKind } ->
    case mbKind of
      Nothing -> " " <> name
      Just kind ->
        " (" <> name <> " "
        <> syntax "::"
        <> space
        <> showKind kind
        <> ")"
  ) <>

  syntax ". " <>
  showType foralls.ty

  where
    foralls = joinForAlls ty


showKind
  :: Kind
  -> String
showKind = case _ of
  Row k1          -> "# " <> showKind k1
  FunKind k1 k2   -> showKind k1 <> syntax " -> " <> showKind k2
  NamedKind qname -> showQualifiedName qname


showConstraint
  :: Constraint
  -> String
showConstraint (Constraint { constraintClass, constraintArgs }) =
  showQualifiedName constraintClass <> space <>
  Array.intercalate space (constraintArgs <#> showType)


syntax :: String -> String
syntax = cyan

space :: String
space = " "

keyword :: String -> String
keyword = cyan
