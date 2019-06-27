module Test.TypeQuery where


import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Argonaut.Parser
import Data.Either
import Data.Maybe
import Effect (Effect)
import Effect.Aff
import Effect.Console (log)
import Partial.Unsafe
import Prelude
import Spago.Search.DocsJson
import Spago.Search.TypeDecoder
import Spago.Search.TypeQuery
import Test.Unit (suite, test, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Data.List as List
import Data.List (List(..), (:))
import Data.Tuple
import Data.Foldable
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList

tests =
  suite "TypeQuery parser" do

    test "test #0" do
      let input = "a"
      assertRight (parseTypeQuery input) (QAny "a")

    test "test #0.1" do
      let input = "ab"
      assertRight (parseTypeQuery input) (QAny "ab")

    test "test #1.0" do
      let input = "a b"
      assertRight (parseTypeQuery input) (QApp (QAny "a") (QAny "b"))

    test "test #1.1" do
      let input = "a b c"
      assertRight (parseTypeQuery input) (QApp (QApp (QAny "a") (QAny "b")) (QAny "c"))

    test "test #2" do
      let input = "a -> b"
      assertRight (parseTypeQuery input) (QFun (QAny "a") (QAny "b"))

    test "test #3" do
      let input = "a -> b c"
      assertRight (parseTypeQuery input) (QFun (QAny "a") (QApp (QAny "b") (QAny "c")))

    test "test #4" do
      let input = "a b -> c"
      assertRight (parseTypeQuery input) (QFun (QApp (QAny "a") (QAny "b")) (QAny "c"))

    test "test #5" do
      let input = "a b"
      assertRight (parseTypeQuery input) (QApp (QAny "a") (QAny "b"))

    test "test #6" do
      let input = "a (b c)"
      assertRight (parseTypeQuery input) (QApp (QAny "a") (QApp (QAny "b") (QAny "c")))

    test "test #6.1" do
      let input = "(a b) (c d)"
      assertRight (parseTypeQuery input)
        (QApp (QApp (QAny "a") (QAny "b"))
              (QApp (QAny "c") (QAny "d")))

    test "test #7" do
      let input = "a ( b c )"
      assertRight (parseTypeQuery input) (QApp (QAny "a") (QApp (QAny "b") (QAny "c")))

    test "test #8" do
      let input = "aaa"
      assertRight (parseTypeQuery input) (QAny "aaa")

    test "test #9" do
      let input = "aaa ( bbb ccc )"
      assertRight (parseTypeQuery input) (QApp (QAny "aaa") (QApp (QAny "bbb") (QAny "ccc")))

    test "test #10" do
      let input = "(a -> b) ->  (c -> d)"
      assertRight (parseTypeQuery input) (QFun (QFun (QAny "a") (QAny "b"))
                                                  (QFun (QAny "c") (QAny "d")))

    test "test #11" do
      let input = "a -> b -> c -> d"
      assertRight (parseTypeQuery input) (QFun (QAny "a")
                                          (QFun (QAny "b")
                                           (QFun (QAny "c") (QAny "d"))))

    test "test #11.1" do
      let input = "a -> b -> c"
      assertRight (parseTypeQuery input) (QFun (QAny "a")
                                          (QFun (QAny "b")
                                           (QAny "c")))

    test "test #12" do
      let input = "forall a b c. c"
      assertRight (parseTypeQuery input) (QForAll (nl "a" ["b", "c"]) (QAny "c"))

    test "test #13" do
      let input = "forall a. Maybe a"
      assertRight (parseTypeQuery input) (QForAll (nl "a" $ []) (QApp (QConcrete "Maybe") (QAny "a")))

    test "test #14" do
      let input = "forall m a. Monad m => a -> m a"
      assertRight (parseTypeQuery input)
        (QForAll (nl "m" ["a"])
                 (QConstraint "Monad" (l ["m"])
                  (QFun (QAny "a")
                        (QApp (QAny "m") (QAny "a")))))

    test "test #15" do
      let input = "{ a :: Int }"
      assertRight (parseTypeQuery input)
        (QRow (pure (Tuple "a" (QConcrete "Int"))))

    test "test #16" do
      let input = "{a::Int}"
      assertRight (parseTypeQuery input)
        (QRow (pure (Tuple "a" (QConcrete "Int"))))

    test "test #17" do
      let input = "Int"
      assertRight (parseTypeQuery input) (QConcrete "Int")

    test "test #18" do
      let input = "a->b"
      assertRight (parseTypeQuery input) (QFun (QAny "a") (QAny "b"))

l :: forall f. Foldable f => (forall a. f a -> List a)
l = List.fromFoldable

nl :: forall t5 t6. Foldable t6 => t5 -> t6 t5 -> NonEmptyList t5
nl x rst = NonEmptyList.cons' x $ List.fromFoldable rst

assertRight
  :: forall a b
  .  Show a
  => Show b
  => Eq a
  => Eq b
  => Either b a
  -> a
  -> Aff Unit
assertRight eiActual expected =
  case eiActual of
    Left string -> do
      Assert.equal (Right expected) eiActual
    Right actual -> do
      Assert.equal (Right expected) eiActual
