module Spago.Search.TypeShape where

import Prelude

import Spago.Search.TypeDecoder
import Spago.Search.TypeQuery
import Spago.Search.TypeQuery

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)

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

        TypeApp (TypeApp (TypeConstructor (QualifiedName { moduleName: ["Prim"]
                                                         , name: "Function" })) t1) t2 ->
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
          go (typesInRow <> rest) (PRow (List.length joined) : acc)
          where
            joined = List.sortBy (\x y -> compare x.row y.row) $ joinRows row
            typesInRow = joined <#> (_.ty)

        BinaryNoParensType op l r ->
          go (TypeApp (TypeApp op l) r : rest) acc

joinForAlls
  :: Type
  -> { binders :: List { var :: String
                       , mbKind :: Maybe Kind }
     , ty :: Type
     }
joinForAlls ty = go Nil ty
  where
    go acc (ForAll var ty' mbKind) =
      go ({ var, mbKind } : acc) ty'
    go acc ty' = { binders: acc, ty: ty' }

joinRows :: Type -> List { row :: String
                         , ty :: Type
                         }
joinRows = go Nil
  where
    go acc (RCons row ty rest) =
      go ({ row, ty } : acc) rest
    go acc _ = List.reverse acc
