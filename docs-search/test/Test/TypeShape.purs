module Test.TypeShape where

import Data.Either
import Data.Either
import Data.Maybe
import Effect.Aff
import Partial.Unsafe
import Prelude
import Spago.Search.DocsJson
import Spago.Search.Index
import Spago.Search.IndexBuilder
import Spago.Search.TypeDecoder
import Spago.Search.TypeQuery
import Spago.Search.TypeShape

import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Search.Trie as Trie
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (exists, readTextFile, readdir, writeTextFile)
import Node.Process as Process
import Test.TypeQuery as TypeQuery
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

-- | Crash-testing query parser on existing types.
run :: Aff Unit
run = do
  paths <- readdir "output"
  declarations <- collectDeclarations paths
  -- liftEffect $ log $ "Found " <> show (Array.length declarations) <> " modules"
  let index = mkSearchIndex declarations
  let types = Trie.values ((unwrap index).types)
  for_ types \tys -> do
    for_ tys \ty -> do
        case (unwrap ty).info of
          ValueResult { type: ty } -> do
            let shown = showType ty
            case parseTypeQuery shown of
              Left err ->
                liftEffect $ log $ show err <> ": \n" <> shown
              _ ->
                pure unit
          _ -> pure unit

showType :: Type -> String
showType = case _ of
  TypeVar _String -> _String
  TypeLevelString _String ->
    _String
  TypeWildcard ->
    "_"
  TypeConstructor _QualifiedName ->
    showQualifiedName _QualifiedName
  TypeOp _QualifiedName ->
    showQualifiedName _QualifiedName
  TypeApp (TypeApp (TypeConstructor (QualifiedName { moduleName: ["Prim"]
                                                   , name: "Function" })) t1) t2 ->
    showType t1 <> " -> " <> showType t2

  TypeApp _Type1 _Type2 ->
    showType _Type1 <> " " <> showType _Type2
  foralls@(ForAll _String _Type  _Maybe_Kind) ->
    let joined = joinForAlls foralls in
    "forall " <> (List.intercalate " " $ joined.binders <#> (_.var)) <> ". " <> showType joined.ty -- <> " " <> show _Maybe_Kind
  ConstrainedType _Constraint _Type ->
    showConstraint _Constraint <> " => " <> showType _Type
  REmpty ->
    "{}"
  RCons _String _Type1 _Type2 ->
       "(RConsLabel " <> _String <> " " <> showType _Type1 <> " " <> showType _Type2 <> ")"
  BinaryNoParensType _TypeOp _Type1 _Type2 ->
    showType _Type1 <> " " <> showType _TypeOp <> " " <> showType _Type2
  ParensInType _Type ->
    "(" <> showType _Type <> ")"

showQualifiedName :: QualifiedName -> String
showQualifiedName = unwrap >>> (_.name)

showConstraint :: Constraint -> String
showConstraint (Constraint { constraintClass, constraintArgs }) =
  showQualifiedName constraintClass <> " " <> Array.intercalate " " (constraintArgs <#> showType)
