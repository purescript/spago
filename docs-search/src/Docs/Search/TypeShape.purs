-- | We need `TypeShape`s as a way to "semantically hash" types.
-- | This allows us to split type index in parts and load
-- | it on demand.
module Docs.Search.TypeShape where

import Docs.Search.TypeDecoder (QualifiedName(..), Type(..), joinForAlls, joinRows)
import Docs.Search.TypeQuery (TypeQuery(..), getFreeVariables)
import Docs.Search.Types (Identifier(..))

import Prelude
import Prim hiding (Type)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import Data.Ord (abs)


type TypeShape = List ShapeChunk


data ShapeChunk
  = PVar
  | PFun
  | PApp
  | PForAll Int
  | PRow Int

derive instance eqShapeChunk :: Eq ShapeChunk
derive instance ordShapeChunk :: Ord ShapeChunk
derive instance genericShapeChunk :: Generic ShapeChunk _

instance showShapeChunk :: Show ShapeChunk where
  show x = genericShow x


stringifyShape :: TypeShape -> String
stringifyShape shape =
  show $ abs $ hash
  if res == "" then "0" else res
  where
    res = List.foldMap stringifyChunk shape
    stringifyChunk =
      case _ of
        PVar      -> "v"
        PFun      -> "f"
        PApp      -> "a"
        PForAll n -> "b" <> show n
        PRow n    -> "r" <> show n


shapeOfTypeQuery :: TypeQuery -> TypeShape
shapeOfTypeQuery query =
  prependForAll $ List.reverse $ go (pure query) Nil
  where

    prependForAll (PForAll n : rest) =
      PForAll (count + n) : rest
    prependForAll shape =
      if count == 0
      then shape
      else PForAll count : shape

    count = Set.size $ getFreeVariables query

    go Nil            acc = acc
    go (this:rest)    acc =
      case this of
        QVar _ ->
          go rest (PVar : acc)
        QConst v ->
          go rest (PVar : acc)
        QFun q1 q2 ->
          go (q1 : q2 : rest) (PFun : acc)
        QApp q1 q2 ->
          go (q1 : q2 : rest) (PApp : acc)
        QForAll lst q ->
          go (q : rest) (PForAll (NonEmptyList.length lst) : acc)
        QConstraint str lst q ->
          go (q : rest) acc
        QRow lst ->
          let lst' = List.sortBy (\(Tuple x _) (Tuple y _) -> compare x y) lst in
          go (map snd lst' <> rest) (PRow (List.length lst) : acc)


shapeOfType :: Type -> TypeShape
shapeOfType ty = List.reverse $ go (pure ty) Nil
  where
    go Nil acc = acc
    go (this:rest) acc =
      case this of

        TypeVar _ ->
          go rest (PVar : acc)

        TypeLevelString _ ->
          go rest (PVar : acc)

        TypeWildcard ->
          go rest (PVar : acc)

        TypeApp (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Prim"]
                                                         , name: Identifier "Function" })) t1) t2 ->
          go (t1 : t2 : rest) (PFun : acc)

        TypeConstructor (QualifiedName { name }) ->
          go rest (PVar : acc)

        TypeOp _ ->
          go rest (PVar : acc)

        TypeApp child1 child2 ->
          go (child1 : child2 : rest) (PApp : acc)

        forallType@(ForAll _ _ _) ->
          go (foralls.ty : rest) (PForAll (List.length foralls.binders) : acc)
          where foralls = joinForAlls forallType

        ParensInType child ->
          go (child : rest) acc

        ConstrainedType _ child ->
          go (child : rest) acc

        REmpty ->
          -- TODO: reconsider
          go rest (PVar : acc)

        row@(RCons _ _ _) ->
          go (typesInRow <> rest) (PRow (List.length joined.rows) : acc)
          where
            joined = joinRows row
            sorted = List.sortBy (\x y -> compare x.row y.row) joined.rows
            typesInRow = sorted <#> (_.ty)

        BinaryNoParensType op l r ->
          go (TypeApp (TypeApp op l) r : rest) acc


foreign import hash :: String -> Int
