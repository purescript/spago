module Test.TypeQuery where

import Prelude

import Test.Extra (assertRight)

import Docs.Search.TypeQuery (Substitution(..), TypeQuery(..), getFreeVariables, parseTypeQuery, penalty, typeVarPenalty)
import Docs.Search.TypeShape (ShapeChunk(..), shapeOfTypeQuery)
import Docs.Search.TypeDecoder (QualifiedName(..), Type(..))

import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = do
  suite "TypeQuery parser" do

    test "test #0" do
      let input = "a"
      assertRight (parseTypeQuery input) (QVar "a")

    test "test #1" do
      let input = "ab"
      assertRight (parseTypeQuery input) (QVar "ab")

    test "test #2" do
      let input = "a b"
      assertRight (parseTypeQuery input) (QApp (QVar "a") (QVar "b"))

    test "test #3" do
      let input = "a b c"
      assertRight (parseTypeQuery input) (QApp (QApp (QVar "a") (QVar "b")) (QVar "c"))

    test "test #4" do
      let input = "a -> b"
      assertRight (parseTypeQuery input) (QFun (QVar "a") (QVar "b"))

    test "test #5" do
      let input = "a -> b c"
      assertRight (parseTypeQuery input) (QFun (QVar "a") (QApp (QVar "b") (QVar "c")))

    test "test #6" do
      let input = "a b -> c"
      assertRight (parseTypeQuery input) (QFun (QApp (QVar "a") (QVar "b")) (QVar "c"))

    test "test #7" do
      let input = "a b"
      assertRight (parseTypeQuery input) (QApp (QVar "a") (QVar "b"))

    test "test #8" do
      let input = "a (b c)"
      assertRight (parseTypeQuery input) (QApp (QVar "a") (QApp (QVar "b") (QVar "c")))

    test "test #9" do
      let input = "(a b) (c d)"
      assertRight (parseTypeQuery input)
        (QApp (QApp (QVar "a") (QVar "b"))
              (QApp (QVar "c") (QVar "d")))

    test "test #10" do
      let input = "a ( b c )"
      assertRight (parseTypeQuery input) (QApp (QVar "a") (QApp (QVar "b") (QVar "c")))

    test "test #11" do
      let input = "aaa"
      assertRight (parseTypeQuery input) (QVar "aaa")

    test "test #12" do
      let input = "aaa ( bbb ccc )"
      assertRight (parseTypeQuery input) (QApp (QVar "aaa") (QApp (QVar "bbb") (QVar "ccc")))

    test "test #13" do
      let input = "(a -> b) ->  (c -> d)"
      assertRight (parseTypeQuery input) (QFun (QFun (QVar "a") (QVar "b"))
                                                  (QFun (QVar "c") (QVar "d")))

    test "test #14" do
      let input = "a -> b -> c -> d"
      assertRight (parseTypeQuery input) (QFun (QVar "a")
                                          (QFun (QVar "b")
                                           (QFun (QVar "c") (QVar "d"))))

    test "test #15" do
      let input = "a -> b -> c"
      assertRight (parseTypeQuery input) (QFun (QVar "a")
                                          (QFun (QVar "b")
                                           (QVar "c")))

    test "test #16" do
      let input = "forall a b c. c"
      assertRight (parseTypeQuery input) (QForAll (nl "a" ["b", "c"]) (QVar "c"))

    test "test #17" do
      let input = "forall a. Maybe a"
      assertRight (parseTypeQuery input) (QForAll (nl "a" $ []) (QApp (QConst "Maybe") (QVar "a")))

    test "test #18" do
      let input = "forall m a. Monad m => a -> m a"
      assertRight (parseTypeQuery input)
        (QForAll (nl "m" ["a"])
                 (QConstraint "Monad" (l [QVar "m"])
                  (QFun (QVar "a")
                        (QApp (QVar "m") (QVar "a")))))

    test "test #19" do
      let input = "{ a :: Int }"
      assertRight (parseTypeQuery input)
        (QApp (QConst "Record") (QRow (pure (Tuple "a" (QConst "Int")))))

    test "test #20" do
      let input = "{a::Int}"
      assertRight (parseTypeQuery input)
        (QApp (QConst "Record") (QRow (pure (Tuple "a" (QConst "Int")))))

    test "test #21" do
      let input = "Int"
      assertRight (parseTypeQuery input) (QConst "Int")

    test "test #22" do
      let input = "a->b"
      assertRight (parseTypeQuery input) (QFun (QVar "a") (QVar "b"))

    test "test #23" do
      let input = "forall m a. MonadRec m => Process m a -> m a"
      assertRight (parseTypeQuery input) (QForAll (nl "m" ("a" : Nil))
                                          (QConstraint "MonadRec" (l [QVar "m"])
                                           (QFun (QApp (QApp (QConst "Process")
                                                        (QVar "m")) (QVar "a"))
                                            (QApp (QVar "m") (QVar "a")))))

    test "test #24" do
      let input = "forall t f a. Foldable1 t => Apply f => f"
      assertRight (parseTypeQuery input) (QForAll (nl "t" ["f", "a"])
                                          (QConstraint "Foldable1" (l [QVar "t"])
                                           (QConstraint "Apply" (l [QVar "f"]) (QVar "f"))))

    test "test #25" do
      let input = "forall m a.MonadRec m=>Process m a->m a"
      assertRight (parseTypeQuery input) ((QForAll (nl "m" ("a" : Nil))
                                           (QConstraint "MonadRec" (l [QVar "m"])
                                            (QFun (QApp (QApp (QConst "Process")
                                                         (QVar "m")) (QVar "a"))
                                             (QApp (QVar "m") (QVar "a"))))))

    test "test #26" do
      let input = "m a -> (a -> m b) -> m b"
      assertRight (parseTypeQuery input) (QFun (QApp (QVar "m") (QVar "a")) (QFun (QFun (QVar "a") (QApp (QVar "m") (QVar "b"))) (QApp (QVar "m") (QVar "b"))))

    test "test #27" do
      let input = "forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)"
      assertRight (parseTypeQuery input) ((QForAll (nl "f" ["a"]))
                                          (QConstraint "Alternative" (l [QVar "f"])
                                           (QConstraint "Lazy" (l [QApp (QVar "f")
                                                                   (QApp (QConst "List") (QVar "a"))])
                                            (QFun (QApp (QVar "f") (QVar "a"))
                                             (QApp (QVar "f")
                                              (QApp (QConst "List") (QVar "a")))))))

    test "test #28" do
      let input = "forall f a. Alternative f => Lazy(f (List a))=>f a -> f (List a)"
      assertRight (parseTypeQuery input) ((QForAll (nl "f" ["a"]))
                                          (QConstraint "Alternative" (l [QVar "f"])
                                           (QConstraint "Lazy" (l [QApp (QVar "f")
                                                                   (QApp (QConst "List") (QVar "a"))])
                                            (QFun (QApp (QVar "f") (QVar "a"))
                                             (QApp (QVar "f")
                                              (QApp (QConst "List") (QVar "a")))))))

    test "test #29" do
      let input = "{a::Int,b::Int}"
      assertRight (parseTypeQuery input)
        (QApp (QConst "Record") (QRow (List.fromFoldable [ Tuple "a" (QConst "Int"), Tuple "b" (QConst "Int")])))

    test "test #30" do
      let input = "{record''' :: Int}"
      assertRight (parseTypeQuery input)
        (QApp (QConst "Record") (QRow (List.fromFoldable [ Tuple "record'''" (QConst "Int")])))

    test "test #31" do
      let input = "(row''' :: Int)"
      assertRight (parseTypeQuery input)
        (QRow (List.fromFoldable [ Tuple "row'''" (QConst "Int")]))

    test "test #32" do
      let input = "(row1 :: Int, row2 :: (),row3::(row4::{}))"
      assertRight (parseTypeQuery input)
        (QRow (l [ Tuple "row1" (QConst "Int")
                 , Tuple "row2" (QRow Nil)
                 , Tuple "row3" (QRow (l [ Tuple "row4" (QApp (QConst "Record") (QRow Nil)) ])) ]))

    test "test #33" do
      let input = "Foldable1 t => Apply f => t (f a) -> f Unit"
      assertRight (parseTypeQuery input)
        (QConstraint "Foldable1" ((QVar "t") : Nil) (QConstraint "Apply" ((QVar "f") : Nil) (QFun (QApp (QVar "t") (QApp (QVar "f") (QVar "a"))) (QApp (QVar "f") (QConst "Unit")))))

    test "test #34" do
      let input = "Foldable1 t => Apply f => t (f a) -> f a"
      assertRight (parseTypeQuery input)
        (QConstraint "Foldable1" ((QVar "t") : Nil) (QConstraint "Apply" ((QVar "f") : Nil) (QFun (QApp (QVar "t") (QApp (QVar "f") (QVar "a"))) (QApp (QVar "f") (QVar "a")))))

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
      Assert.equal 0 (typeVarPenalty $ l [ Substitute "a" "b"
                                         , Substitute "b" "a"
                                         ])

    test "#2" do
      Assert.equal 0 (typeVarPenalty $ l [ Substitute "a" "b"
                                         , Substitute "a" "b"
                                         , Substitute "a" "b"
                                         ])

    test "#3" do
      Assert.equal 1 (typeVarPenalty $ l [ Substitute "a" "b"
                                         , Substitute "a" "c"
                                         ])

    test "#4" do
      Assert.equal 1 (typeVarPenalty $ l [ Substitute "a" "b"
                                         , Substitute "b" "a"
                                         , Substitute "b" "c"
                                         ])

    test "#5" do
      Assert.equal 0 (typeVarPenalty $ l [ Substitute "a" "b"
                                         , Substitute "b" "c"
                                         , Substitute "c" "a"
                                         ])

    test "#6" do
      Assert.equal 2 (typeVarPenalty $ l [ Substitute "a" "b"
                                         , Substitute "a" "c"
                                         , Substitute "a" "a"
                                         ])

    test "#7" do
      Assert.equal 2 (typeVarPenalty $ l [ Substitute "a" "a"
                                         , Substitute "b" "a"
                                         , Substitute "c" "a"
                                         ])

    test "#8" do
      Assert.equal 4 (typeVarPenalty $ l [ Substitute "a" "a"
                                         , Substitute "b" "a"
                                         , Substitute "c" "a"
                                         , Substitute "a" "b"
                                         , Substitute "a" "c"
                                         , Substitute "a" "a"
                                         ])

    test "#9" do
      Assert.equal 0 (typeVarPenalty $ l [ Substitute "a" "e"
                                         , Substitute "b" "d"
                                         , Substitute "c" "f"
                                         ])

  suite "unification" do
    test "instantiation #0" do
      let mVarQuery = QVar "m"
          unitConstQuery = QConst "Unit"

      Assert.assert "instantiation #0" $
        (penalty unitConstQuery unitType < penalty mVarQuery unitType)

    test "generalization #0" do
      let query = QVar "m"
          t1 = TypeVar "m"

      Assert.assert "qeneralization #0" $
        (penalty query unitType > penalty query t1)


l :: forall f. Foldable f => (forall a. f a -> List a)
l = List.fromFoldable

nl :: forall t5 t6. Foldable t6 => t5 -> t6 t5 -> NonEmptyList t5
nl x rst = NonEmptyList.cons' x $ List.fromFoldable rst

unitType :: Type
unitType = TypeConstructor (QualifiedName { moduleName: []
                                          , name: "Unit"
                                          })

countFreeVars :: TypeQuery -> Int
countFreeVars = getFreeVariables >>> Set.size
