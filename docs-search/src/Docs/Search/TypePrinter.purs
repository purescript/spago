module Docs.Search.TypePrinter where

import Prelude
import Prim hiding (Type, Constraint)
import Docs.Search.Extra ((>#>))
import Docs.Search.Terminal (cyan)
import Docs.Search.TypeDecoder (Constraint(..), FunDep(..), FunDeps(..), QualifiedName(..), Type(..), TypeArgument(..), joinForAlls, joinRows)
import Docs.Search.Types (Identifier(..))

import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)


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
                                   , name: Identifier "Function" }))
                   t1)
          t2 ->
    showType t1 <> syntax " -> " <> showType t2

  TypeApp (TypeConstructor (QualifiedName { moduleNameParts: [ "Prim" ]
                                          , name: Identifier "Record" }))
          row ->
    showRow false row

  TypeApp t1 t2 ->
    showType t1 <> " " <> showType t2

  KindApp t1 t2 ->
    showType t1 <> " " <> showType t2

  ty@(ForAll _ _ _) ->
    showForAll ty

  ConstrainedType cnstr ty ->
    showConstraint cnstr <>
    syntax " => " <>
    showType ty

  ty@REmpty -> showRow true ty
  ty@(RCons _ _ _) -> showRow true ty

  Kinded t1 t2 -> showType t1 <> " :: " <> showType t2

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
      showType kind <>
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
  = unwrap name


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
      unwrap entry.row <> syntax " :: " <> showType entry.ty
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

  ( Array.fold $ Array.fromFoldable $ foralls.binders <#>
    \ { name, mbKind } ->
    case mbKind of
      Nothing -> " " <> name
      Just kind ->
        " (" <> name <> " "
        <> syntax "::"
        <> space
        <> showType kind
        <> ")"
  ) <>

  syntax ". " <>
  showType foralls.ty

  where
    foralls = joinForAlls ty


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
