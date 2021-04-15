module Test.TypeQuery where

import Docs.Search.TypeDecoder (Constraint(..), QualifiedName(..), Type(..))
import Docs.Search.TypeQuery (Substitution(..), TypeQuery(..), getFreeVariables, parseTypeQuery, penalty, typeVarPenalty)
import Docs.Search.TypeShape (ShapeChunk(..), shapeOfType, shapeOfTypeQuery)
import Docs.Search.Types (Identifier(..))

import Prelude
import Prim hiding (Constraint, Type)
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Test.Extra (assertRight)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Data.Maybe (Maybe(..))


tests :: TestSuite
tests = do
  suite "TypeQuery parser" do

    test "test #0" do
      let input = "a"
      assertRight (parseTypeQuery input) (qVar "a")

    test "test #1" do
      let input = "ab"
      assertRight (parseTypeQuery input) (qVar "ab")

    test "test #2" do
      let input = "a b"
      assertRight (parseTypeQuery input) (QApp (qVar "a") (qVar "b"))

    test "test #3" do
      let input = "a b c"
      assertRight (parseTypeQuery input) (QApp (QApp (qVar "a") (qVar "b")) (qVar "c"))

    test "test #4" do
      let input = "a -> b"
      assertRight (parseTypeQuery input) (QFun (qVar "a") (qVar "b"))

    test "test #5" do
      let input = "a -> b c"
      assertRight (parseTypeQuery input) (QFun (qVar "a") (QApp (qVar "b") (qVar "c")))

    test "test #6" do
      let input = "a b -> c"
      assertRight (parseTypeQuery input) (QFun (QApp (qVar "a") (qVar "b")) (qVar "c"))

    test "test #7" do
      let input = "a b"
      assertRight (parseTypeQuery input) (QApp (qVar "a") (qVar "b"))

    test "test #8" do
      let input = "a (b c)"
      assertRight (parseTypeQuery input) (QApp (qVar "a") (QApp (qVar "b") (qVar "c")))

    test "test #9" do
      let input = "(a b) (c d)"
      assertRight (parseTypeQuery input)
        (QApp (QApp (qVar "a") (qVar "b"))
              (QApp (qVar "c") (qVar "d")))

    test "test #10" do
      let input = "a ( b c )"
      assertRight (parseTypeQuery input) (QApp (qVar "a") (QApp (qVar "b") (qVar "c")))

    test "test #11" do
      let input = "aaa"
      assertRight (parseTypeQuery input) (qVar "aaa")

    test "test #12" do
      let input = "aaa ( bbb ccc )"
      assertRight (parseTypeQuery input) (QApp (qVar "aaa") (QApp (qVar "bbb") (qVar "ccc")))

    test "test #13" do
      let input = "(a -> b) ->  (c -> d)"
      assertRight (parseTypeQuery input) (QFun (QFun (qVar "a") (qVar "b"))
                                                  (QFun (qVar "c") (qVar "d")))

    test "test #14" do
      let input = "a -> b -> c -> d"
      assertRight (parseTypeQuery input) (QFun (qVar "a")
                                          (QFun (qVar "b")
                                           (QFun (qVar "c") (qVar "d"))))

    test "test #15" do
      let input = "a -> b -> c"
      assertRight (parseTypeQuery input) (QFun (qVar "a")
                                          (QFun (qVar "b")
                                           (qVar "c")))

    test "test #16" do
      let input = "forall a b c. c"
      assertRight (parseTypeQuery input) (QForAll (nl "a" ["b", "c"]) (qVar "c"))

    test "test #17" do
      let input = "forall a. Maybe a"
      assertRight (parseTypeQuery input) (QForAll (nl "a" $ []) (QApp (qConst "Maybe") (qVar "a")))

    test "test #18" do
      let input = "forall m a. Monad m => a -> m a"
      assertRight (parseTypeQuery input)
        (QForAll (nl "m" ["a"])
                 (qConstraint "Monad" (l [qVar "m"])
                  (QFun (qVar "a")
                        (QApp (qVar "m") (qVar "a")))))

    test "test #19" do
      let input = "{ a :: Int }"
      assertRight (parseTypeQuery input)
        (QApp (qConst "Record") (QRow (pure (Tuple (Identifier "a") (qConst "Int")))))

    test "test #20" do
      let input = "{a::Int}"
      assertRight (parseTypeQuery input)
        (QApp (qConst "Record") (QRow (pure (Tuple (Identifier "a") (qConst "Int")))))

    test "test #21" do
      let input = "Int"
      assertRight (parseTypeQuery input) (qConst "Int")

    test "test #22" do
      let input = "a->b"
      assertRight (parseTypeQuery input) (QFun (qVar "a") (qVar "b"))

    test "test #23" do
      let input = "forall m a. MonadRec m => Process m a -> m a"
      assertRight (parseTypeQuery input) (QForAll (nl "m" ("a" : Nil))
                                          (qConstraint "MonadRec" (l [qVar "m"])
                                           (QFun (QApp (QApp (qConst "Process")
                                                        (qVar "m")) (qVar "a"))
                                            (QApp (qVar "m") (qVar "a")))))

    test "test #24" do
      let input = "forall t f a. Foldable1 t => Apply f => f"
      assertRight (parseTypeQuery input) (QForAll (nl "t" ["f", "a"])
                                          (qConstraint "Foldable1" (l [qVar "t"])
                                           (qConstraint "Apply" (l [qVar "f"]) (qVar "f"))))

    test "test #25" do
      let input = "forall m a.MonadRec m=>Process m a->m a"
      assertRight (parseTypeQuery input) ((QForAll (nl "m" ("a" : Nil))
                                           (qConstraint "MonadRec" (l [qVar "m"])
                                            (QFun (QApp (QApp (qConst "Process")
                                                         (qVar "m")) (qVar "a"))
                                             (QApp (qVar "m") (qVar "a"))))))

    test "test #26" do
      let input = "m a -> (a -> m b) -> m b"
      assertRight (parseTypeQuery input) (QFun (QApp (qVar "m") (qVar "a")) (QFun (QFun (qVar "a") (QApp (qVar "m") (qVar "b"))) (QApp (qVar "m") (qVar "b"))))

    test "test #27" do
      let input = "forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)"
      assertRight (parseTypeQuery input) ((QForAll (nl "f" ["a"]))
                                          (qConstraint "Alternative" (l [qVar "f"])
                                           (qConstraint "Lazy" (l [QApp (qVar "f")
                                                                   (QApp (qConst "List") (qVar "a"))])
                                            (QFun (QApp (qVar "f") (qVar "a"))
                                             (QApp (qVar "f")
                                              (QApp (qConst "List") (qVar "a")))))))

    test "test #28" do
      let input = "forall f a. Alternative f => Lazy(f (List a))=>f a -> f (List a)"
      assertRight (parseTypeQuery input) ((QForAll (nl "f" ["a"]))
                                          (qConstraint "Alternative" (l [qVar "f"])
                                           (qConstraint "Lazy" (l [QApp (qVar "f")
                                                                   (QApp (qConst "List") (qVar "a"))])
                                            (QFun (QApp (qVar "f") (qVar "a"))
                                             (QApp (qVar "f")
                                              (QApp (qConst "List") (qVar "a")))))))

    test "test #29" do
      let input = "{a::Int,b::Int}"
      assertRight (parseTypeQuery input)
        (QApp (qConst "Record") (QRow (List.fromFoldable
                                       [ Tuple (Identifier "a") (qConst "Int")
                                       , Tuple (Identifier "b") (qConst "Int")])))

    test "test #30" do
      let input = "{record''' :: Int}"
      assertRight (parseTypeQuery input)
        (QApp (qConst "Record") (QRow (List.fromFoldable [ Tuple (Identifier "record'''") (qConst "Int")])))

    test "test #31" do
      let input = "(row''' :: Int)"
      assertRight (parseTypeQuery input)
        (QRow (List.fromFoldable [ Tuple (Identifier "row'''") (qConst "Int")]))

    test "test #32" do
      let input = "(row1 :: Int, row2 :: (),row3::(row4::{}))"
      assertRight (parseTypeQuery input)
        (QRow (l [ Tuple (Identifier "row1") (qConst "Int")
                 , Tuple (Identifier "row2") (QRow Nil)
                 , Tuple (Identifier "row3") (QRow (l [ Tuple (Identifier "row4") (QApp (qConst "Record") (QRow Nil)) ])) ]))

    test "test #33" do
      let input = "Foldable1 t => Apply f => t (f a) -> f Unit"
      assertRight (parseTypeQuery input)
        (qConstraint "Foldable1" ((qVar "t") : Nil) (qConstraint "Apply" ((qVar "f") : Nil) (QFun (QApp (qVar "t") (QApp (qVar "f") (qVar "a"))) (QApp (qVar "f") (qConst "Unit")))))

    test "test #34" do
      let input = "Foldable1 t => Apply f => t (f a) -> f a"
      assertRight (parseTypeQuery input)
        (qConstraint "Foldable1" ((qVar "t") : Nil) (qConstraint "Apply" ((qVar "f") : Nil) (QFun (QApp (qVar "t") (QApp (qVar "f") (qVar "a"))) (QApp (qVar "f") (qVar "a")))))

    test "test #35" do
      let input = "Generic a rep => GenericEq rep => a -> a -> Boolean"
      assertRight (parseTypeQuery input)
        (qConstraint "Generic" ((qVar "a") : (qVar "rep") : Nil)
         (qConstraint "GenericEq" ((qVar "rep") : Nil)
          (QFun (qVar "a") (QFun (qVar "a") (qConst "Boolean")))))

  suite "polish notation" do

    test "test #1" do
      let input = "(a -> b) -> (b -> ((a -> b) -> c))"
      assertRight (shapeOfTypeQuery <$> parseTypeQuery input)
          (l [ PForAll 3, PFun, PFun, PVar, PVar, PFun, PVar, PFun, PFun
             , PVar, PVar, PVar ])

    test "test #2" do
      let input = "forall a. (a -> b) -> (b -> ((a -> b) -> c))"
      assertRight (shapeOfTypeQuery <$> parseTypeQuery input)
          (l [ PForAll 3, PFun, PFun, PVar, PVar, PFun, PVar, PFun, PFun
             , PVar, PVar, PVar ])

    test "test #3" do
      let input = "forall a. (a -> b) -> (b -> ((a -> b) -> c))"
      assertRight (shapeOfTypeQuery <$> parseTypeQuery input)
          (l [ PForAll 3, PFun, PFun, PVar, PVar, PFun, PVar, PFun, PFun
             , PVar, PVar, PVar ])

    test "test #4" do
      let input = "forall a. (forall h. ST h (STArray h a)) -> Array a"
      assertRight (shapeOfTypeQuery <$> parseTypeQuery input)
          (l [ PForAll 1, PFun, PForAll 1, PApp, PApp, PVar, PVar, PApp, PApp, PVar, PVar, PVar, PApp, PVar, PVar ])

  suite "type shapes" do
    test "test #1" do
      let query = "Generic a rep => GenericEq rep => a -> a -> Boolean"
          c1 = constr (qname [""] "Generic") [TypeVar "a", TypeVar "rep"]
          c2 = constr (qname [""] "GenericEq") [TypeVar "rep"]

          fun t1 t2 =
            TypeApp (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Prim"]
                                                             , name: Identifier "Function" })) t1) t2
          type_ =
            ForAll "a" Nothing $
            ForAll "rep" Nothing $
            ConstrainedType c1
            (ConstrainedType c2
             (fun (TypeVar "a") (fun (TypeVar "b")
                                 (TypeConstructor $ qname ["Prim", "Boolean"] "Boolean"))))
          shape = shapeOfTypeQuery <$> parseTypeQuery query
      Assert.equal (pure $ shapeOfType type_) shape

  suite "free variable counting" do

    test "test #1" do
      let input = "forall a. (a -> b) -> (b -> ((a -> b) -> c))"
      assertRight (countFreeVars <$> parseTypeQuery input) 2

    test "test #2" do
      -- `b` is not bound on the left, `a` is not bound on the right
      let input = "(forall a. (a -> b)) -> forall b. (b -> a)"
      assertRight (countFreeVars <$> parseTypeQuery input) 2

    test "test #3" do
      let input = "a -> forall a. a"
      assertRight (countFreeVars <$> parseTypeQuery input) 1

    test "test #4" do
      let input = "(forall a. a) -> a"
      assertRight (countFreeVars <$> parseTypeQuery input) 1

    test "test #5" do
      let input = "forall a. a -> a"
      assertRight (countFreeVars <$> parseTypeQuery input) 0

    test "test #6" do
      let input = "a -> b -> c"
      assertRight (countFreeVars <$> parseTypeQuery input) 3

    test "test #7" do
      let input = "forall m a. Monad m => a -> m a"
      assertRight (countFreeVars <$> parseTypeQuery input) 0

    test "test #8" do
      let input = "Monad m => a -> m a"
      assertRight (countFreeVars <$> parseTypeQuery input) 2

    test "test #9" do
      let input = "Monad m => a -> a"
      assertRight (countFreeVars <$> parseTypeQuery input) 2

    test "test #10" do
      let input = "forall a. (forall a. a) a"
      assertRight (countFreeVars <$> parseTypeQuery input) 0

    test "test #11" do
      let input = "forall a. (forall b. a) a"
      assertRight (countFreeVars <$> parseTypeQuery input) 0

    test "test #12" do
      let input = "forall a. (forall b. a) a b"
      assertRight (countFreeVars <$> parseTypeQuery input) 1

  suite "typeVarPenalty" do

    test "#0" do
      Assert.equal 0 (typeVarPenalty mempty)

    test "#1" do
      Assert.equal 0 (typeVarPenalty $ l [ substitute "a" "b"
                                         , substitute "b" "a"
                                         ])

    test "#2" do
      Assert.equal 0 (typeVarPenalty $ l [ substitute "a" "b"
                                         , substitute "a" "b"
                                         , substitute "a" "b"
                                         ])

    test "#3" do
      Assert.equal 1 (typeVarPenalty $ l [ substitute "a" "b"
                                         , substitute "a" "c"
                                         ])

    test "#4" do
      Assert.equal 1 (typeVarPenalty $ l [ substitute "a" "b"
                                         , substitute "b" "a"
                                         , substitute "b" "c"
                                         ])

    test "#5" do
      Assert.equal 0 (typeVarPenalty $ l [ substitute "a" "b"
                                         , substitute "b" "c"
                                         , substitute "c" "a"
                                         ])

    test "#6" do
      Assert.equal 2 (typeVarPenalty $ l [ substitute "a" "b"
                                         , substitute "a" "c"
                                         , substitute "a" "a"
                                         ])

    test "#7" do
      Assert.equal 2 (typeVarPenalty $ l [ substitute "a" "a"
                                         , substitute "b" "a"
                                         , substitute "c" "a"
                                         ])

    test "#8" do
      Assert.equal 4 (typeVarPenalty $ l [ substitute "a" "a"
                                         , substitute "b" "a"
                                         , substitute "c" "a"
                                         , substitute "a" "b"
                                         , substitute "a" "c"
                                         , substitute "a" "a"
                                         ])

    test "#9" do
      Assert.equal 0 (typeVarPenalty $ l [ substitute "a" "e"
                                         , substitute "b" "d"
                                         , substitute "c" "f"
                                         ])

  suite "unification" do
    test "instantiation #0" do
      let mVarQuery = qVar "m"
          unitConstQuery = qConst "Unit"

      Assert.assert "instantiation #0" $
        (penalty unitConstQuery unitType < penalty mVarQuery unitType)

    test "generalization #0" do
      let query = qVar "m"
          t1 = TypeVar "m"

      Assert.assert "qeneralization #0" $
        (penalty query unitType > penalty query t1)


l :: forall f. Foldable f => (forall a. f a -> List a)
l = List.fromFoldable

nl
  :: forall t
  .  Foldable t
  => Functor t
  => String
  -> t String
  -> NonEmptyList Identifier
nl x rst = NonEmptyList.cons' (Identifier x) $ List.fromFoldable (rst <#> Identifier)

unitType :: Type
unitType = TypeConstructor (QualifiedName { moduleNameParts: []
                                          , name: Identifier "Unit"
                                          })

countFreeVars :: TypeQuery -> Int
countFreeVars = getFreeVariables >>> Set.size

qname :: Array String -> String -> QualifiedName
qname m n = QualifiedName { moduleNameParts: m, name: Identifier n }

constr :: QualifiedName -> Array Type -> Constraint
constr c a = Constraint { constraintClass: c, constraintArgs: a }

qVar :: String -> TypeQuery
qVar = QVar <<< Identifier

qConst :: String -> TypeQuery
qConst = QConst <<< Identifier

qConstraint :: String -> List TypeQuery -> TypeQuery -> TypeQuery
qConstraint = QConstraint <<< Identifier

substitute :: String -> String -> Substitution
substitute a b = Substitute (Identifier a) (Identifier b)
