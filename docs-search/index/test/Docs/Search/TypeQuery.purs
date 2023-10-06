module Test.TypeQuery where

import Docs.Search.TypeDecoder (Constraint(..), Qualified(..), QualifiedBy(..), Type(..), ProperName(..), ClassName, ModuleName(..), Type', Constraint', TypeVarVisibility(..))
import Docs.Search.TypeQuery (Substitution(..), TypeQuery(..), getFreeVariables, parseTypeQuery, penalty, typeVarPenalty)
import Docs.Search.TypeShape (ShapeChunk(..), shapeOfType, shapeOfTypeQuery)
import Docs.Search.Types (Identifier(..))

import Prelude
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Set as Set
import Data.String.Common as String
import Data.Tuple (Tuple(..))
import Test.Extra (assertRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Data.Maybe (Maybe(..))

tests :: Spec Unit
tests = do
  describe "TypeQuery parser" $ do
    it "test #0" do
      let input = "a"
      assertRight (parseTypeQuery input) (qVar "a")

    it "test #1" do
      let input = "ab"
      assertRight (parseTypeQuery input) (qVar "ab")

    it "test #2" do
      let input = "a b"
      assertRight (parseTypeQuery input) (QApp (qVar "a") (qVar "b"))

    it "test #3" do
      let input = "a b c"
      assertRight (parseTypeQuery input) (QApp (QApp (qVar "a") (qVar "b")) (qVar "c"))

    it "test #4" do
      let input = "a -> b"
      assertRight (parseTypeQuery input) (QFun (qVar "a") (qVar "b"))

    it "test #5" do
      let input = "a -> b c"
      assertRight (parseTypeQuery input) (QFun (qVar "a") (QApp (qVar "b") (qVar "c")))

    it "test #6" do
      let input = "a b -> c"
      assertRight (parseTypeQuery input) (QFun (QApp (qVar "a") (qVar "b")) (qVar "c"))

    it "test #7" do
      let input = "a b"
      assertRight (parseTypeQuery input) (QApp (qVar "a") (qVar "b"))

    it "test #8" do
      let input = "a (b c)"
      assertRight (parseTypeQuery input) (QApp (qVar "a") (QApp (qVar "b") (qVar "c")))

    it "test #9" do
      let input = "(a b) (c d)"
      assertRight (parseTypeQuery input)
        ( QApp (QApp (qVar "a") (qVar "b"))
            (QApp (qVar "c") (qVar "d"))
        )

    it "test #10" do
      let input = "a ( b c )"
      assertRight (parseTypeQuery input) (QApp (qVar "a") (QApp (qVar "b") (qVar "c")))

    it "test #11" do
      let input = "aaa"
      assertRight (parseTypeQuery input) (qVar "aaa")

    it "test #12" do
      let input = "aaa ( bbb ccc )"
      assertRight (parseTypeQuery input) (QApp (qVar "aaa") (QApp (qVar "bbb") (qVar "ccc")))

    it "test #13" do
      let input = "(a -> b) ->  (c -> d)"
      assertRight (parseTypeQuery input)
        ( QFun (QFun (qVar "a") (qVar "b"))
            (QFun (qVar "c") (qVar "d"))
        )

    it "test #14" do
      let input = "a -> b -> c -> d"
      assertRight (parseTypeQuery input)
        ( QFun (qVar "a")
            ( QFun (qVar "b")
                (QFun (qVar "c") (qVar "d"))
            )
        )

    it "test #15" do
      let input = "a -> b -> c"
      assertRight (parseTypeQuery input)
        ( QFun (qVar "a")
            ( QFun (qVar "b")
                (qVar "c")
            )
        )

    it "test #16" do
      let input = "forall a b c. c"
      assertRight (parseTypeQuery input) (QForAll (nl "a" [ "b", "c" ]) (qVar "c"))

    it "test #17" do
      let input = "forall a. Maybe a"
      assertRight (parseTypeQuery input) (QForAll (nl "a" $ []) (QApp (qConst "Maybe") (qVar "a")))

    it "test #18" do
      let input = "forall m a. Monad m => a -> m a"
      assertRight (parseTypeQuery input)
        ( QForAll (nl "m" [ "a" ])
            ( qConstraint "Monad" (l [ qVar "m" ])
                ( QFun (qVar "a")
                    (QApp (qVar "m") (qVar "a"))
                )
            )
        )

    it "test #19" do
      let input = "{ a :: Int }"
      assertRight (parseTypeQuery input)
        (QApp (qConst "Record") (QRow (pure (Tuple (Identifier "a") (qConst "Int")))))

    it "test #20" do
      let input = "{a::Int}"
      assertRight (parseTypeQuery input)
        (QApp (qConst "Record") (QRow (pure (Tuple (Identifier "a") (qConst "Int")))))

    it "test #21" do
      let input = "Int"
      assertRight (parseTypeQuery input) (qConst "Int")

    it "test #22" do
      let input = "a->b"
      assertRight (parseTypeQuery input) (QFun (qVar "a") (qVar "b"))

    it "test #23" do
      let input = "forall m a. MonadRec m => Process m a -> m a"
      assertRight (parseTypeQuery input)
        ( QForAll (nl "m" ("a" : Nil))
            ( qConstraint "MonadRec" (l [ qVar "m" ])
                ( QFun
                    ( QApp
                        ( QApp (qConst "Process")
                            (qVar "m")
                        )
                        (qVar "a")
                    )
                    (QApp (qVar "m") (qVar "a"))
                )
            )
        )

    it "test #24" do
      let input = "forall t f a. Foldable1 t => Apply f => f"
      assertRight (parseTypeQuery input)
        ( QForAll (nl "t" [ "f", "a" ])
            ( qConstraint "Foldable1" (l [ qVar "t" ])
                (qConstraint "Apply" (l [ qVar "f" ]) (qVar "f"))
            )
        )

    it "test #25" do
      let input = "forall m a.MonadRec m=>Process m a->m a"
      assertRight (parseTypeQuery input)
        ( ( QForAll (nl "m" ("a" : Nil))
              ( qConstraint "MonadRec" (l [ qVar "m" ])
                  ( QFun
                      ( QApp
                          ( QApp (qConst "Process")
                              (qVar "m")
                          )
                          (qVar "a")
                      )
                      (QApp (qVar "m") (qVar "a"))
                  )
              )
          )
        )

    it "test #26" do
      let input = "m a -> (a -> m b) -> m b"
      assertRight (parseTypeQuery input) (QFun (QApp (qVar "m") (qVar "a")) (QFun (QFun (qVar "a") (QApp (qVar "m") (qVar "b"))) (QApp (qVar "m") (qVar "b"))))

    it "test #27" do
      let input = "forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)"
      assertRight (parseTypeQuery input)
        ( (QForAll (nl "f" [ "a" ]))
            ( qConstraint "Alternative" (l [ qVar "f" ])
                ( qConstraint "Lazy"
                    ( l
                        [ QApp (qVar "f")
                            (QApp (qConst "List") (qVar "a"))
                        ]
                    )
                    ( QFun (QApp (qVar "f") (qVar "a"))
                        ( QApp (qVar "f")
                            (QApp (qConst "List") (qVar "a"))
                        )
                    )
                )
            )
        )

    it "test #28" do
      let input = "forall f a. Alternative f => Lazy(f (List a))=>f a -> f (List a)"
      assertRight (parseTypeQuery input)
        ( (QForAll (nl "f" [ "a" ]))
            ( qConstraint "Alternative" (l [ qVar "f" ])
                ( qConstraint "Lazy"
                    ( l
                        [ QApp (qVar "f")
                            (QApp (qConst "List") (qVar "a"))
                        ]
                    )
                    ( QFun (QApp (qVar "f") (qVar "a"))
                        ( QApp (qVar "f")
                            (QApp (qConst "List") (qVar "a"))
                        )
                    )
                )
            )
        )

    it "test #29" do
      let input = "{a::Int,b::Int}"
      assertRight (parseTypeQuery input)
        ( QApp (qConst "Record")
            ( QRow
                ( List.fromFoldable
                    [ Tuple (Identifier "a") (qConst "Int")
                    , Tuple (Identifier "b") (qConst "Int")
                    ]
                )
            )
        )

    it "test #30" do
      let input = "{record''' :: Int}"
      assertRight (parseTypeQuery input)
        (QApp (qConst "Record") (QRow (List.fromFoldable [ Tuple (Identifier "record'''") (qConst "Int") ])))

    it "test #31" do
      let input = "(row''' :: Int)"
      assertRight (parseTypeQuery input)
        (QRow (List.fromFoldable [ Tuple (Identifier "row'''") (qConst "Int") ]))

    it "test #32" do
      let input = "(row1 :: Int, row2 :: (),row3::(row4::{}))"
      assertRight (parseTypeQuery input)
        ( QRow
            ( l
                [ Tuple (Identifier "row1") (qConst "Int")
                , Tuple (Identifier "row2") (QRow Nil)
                , Tuple (Identifier "row3") (QRow (l [ Tuple (Identifier "row4") (QApp (qConst "Record") (QRow Nil)) ]))
                ]
            )
        )

    it "test #33" do
      let input = "Foldable1 t => Apply f => t (f a) -> f Unit"
      assertRight (parseTypeQuery input)
        (qConstraint "Foldable1" ((qVar "t") : Nil) (qConstraint "Apply" ((qVar "f") : Nil) (QFun (QApp (qVar "t") (QApp (qVar "f") (qVar "a"))) (QApp (qVar "f") (qConst "Unit")))))

    it "test #34" do
      let input = "Foldable1 t => Apply f => t (f a) -> f a"
      assertRight (parseTypeQuery input)
        (qConstraint "Foldable1" ((qVar "t") : Nil) (qConstraint "Apply" ((qVar "f") : Nil) (QFun (QApp (qVar "t") (QApp (qVar "f") (qVar "a"))) (QApp (qVar "f") (qVar "a")))))

    it "test #35" do
      let input = "Generic a rep => GenericEq rep => a -> a -> Boolean"
      assertRight (parseTypeQuery input)
        ( qConstraint "Generic" ((qVar "a") : (qVar "rep") : Nil)
            ( qConstraint "GenericEq" ((qVar "rep") : Nil)
                (QFun (qVar "a") (QFun (qVar "a") (qConst "Boolean")))
            )
        )

  describe "polish notation" do
    it "test #1" do
      let input = "(a -> b) -> (b -> ((a -> b) -> c))"
      assertRight (shapeOfTypeQuery <$> parseTypeQuery input)
        ( l
            [ PForAll 3
            , PFun
            , PFun
            , PVar
            , PVar
            , PFun
            , PVar
            , PFun
            , PFun
            , PVar
            , PVar
            , PVar
            ]
        )

    it "test #2" do
      let input = "forall a. (a -> b) -> (b -> ((a -> b) -> c))"
      assertRight (shapeOfTypeQuery <$> parseTypeQuery input)
        ( l
            [ PForAll 3
            , PFun
            , PFun
            , PVar
            , PVar
            , PFun
            , PVar
            , PFun
            , PFun
            , PVar
            , PVar
            , PVar
            ]
        )

    it "test #3" do
      let input = "forall a. (a -> b) -> (b -> ((a -> b) -> c))"
      assertRight (shapeOfTypeQuery <$> parseTypeQuery input)
        ( l
            [ PForAll 3
            , PFun
            , PFun
            , PVar
            , PVar
            , PFun
            , PVar
            , PFun
            , PFun
            , PVar
            , PVar
            , PVar
            ]
        )

    it "test #4" do
      let input = "forall a. (forall h. ST h (STArray h a)) -> Array a"
      assertRight (shapeOfTypeQuery <$> parseTypeQuery input)
        (l [ PForAll 1, PFun, PForAll 1, PApp, PApp, PVar, PVar, PApp, PApp, PVar, PVar, PVar, PApp, PVar, PVar ])

  describe "type shapes" do
    it "test #1" do
      let
        query = "Generic a rep => GenericEq rep => a -> a -> Boolean"
        c1 = constraint (qname [ "" ] "Generic") [ tyVar "a", tyVar "rep" ]
        c2 = constraint (qname [ "" ] "GenericEq") [ tyVar "rep" ]

        fun t1 t2 =
          TypeApp unit
            ( TypeApp unit
                ( TypeConstructor unit
                    ( qname [ "Prim" ] "Function"
                    )
                )
                t1
            )
            t2
        type_ =
          tyForAll "a"
            $ tyForAll "rep"
            $ ConstrainedType unit c1
                ( ConstrainedType unit c2
                    ( fun (tyVar "a")
                        ( fun (tyVar "b")
                            (TypeConstructor unit $ qname [ "Prim", "Boolean" ] "Boolean")
                        )
                    )
                )
        shape = shapeOfTypeQuery <$> parseTypeQuery query
      pure (shapeOfType type_) `shouldEqual` shape

  describe "free variable counting" do

    it "test #1" do
      let input = "forall a. (a -> b) -> (b -> ((a -> b) -> c))"
      assertRight (countFreeVars <$> parseTypeQuery input) 2

    it "test #2" do
      -- `b` is not bound on the left, `a` is not bound on the right
      let input = "(forall a. (a -> b)) -> forall b. (b -> a)"
      assertRight (countFreeVars <$> parseTypeQuery input) 2

    it "test #3" do
      let input = "a -> forall a. a"
      assertRight (countFreeVars <$> parseTypeQuery input) 1

    it "test #4" do
      let input = "(forall a. a) -> a"
      assertRight (countFreeVars <$> parseTypeQuery input) 1

    it "test #5" do
      let input = "forall a. a -> a"
      assertRight (countFreeVars <$> parseTypeQuery input) 0

    it "test #6" do
      let input = "a -> b -> c"
      assertRight (countFreeVars <$> parseTypeQuery input) 3

    it "test #7" do
      let input = "forall m a. Monad m => a -> m a"
      assertRight (countFreeVars <$> parseTypeQuery input) 0

    it "test #8" do
      let input = "Monad m => a -> m a"
      assertRight (countFreeVars <$> parseTypeQuery input) 2

    it "test #9" do
      let input = "Monad m => a -> a"
      assertRight (countFreeVars <$> parseTypeQuery input) 2

    it "test #10" do
      let input = "forall a. (forall a. a) a"
      assertRight (countFreeVars <$> parseTypeQuery input) 0

    it "test #11" do
      let input = "forall a. (forall b. a) a"
      assertRight (countFreeVars <$> parseTypeQuery input) 0

    it "test #12" do
      let input = "forall a. (forall b. a) a b"
      assertRight (countFreeVars <$> parseTypeQuery input) 1

  describe "typeVarPenalty" do

    it "#0" do
      0 `shouldEqual` (typeVarPenalty mempty)

    it "#1" do
      0 `shouldEqual`
        ( typeVarPenalty $ l
            [ substitute "a" "b"
            , substitute "b" "a"
            ]
        )

    it "#2" do
      0 `shouldEqual`
        ( typeVarPenalty $ l
            [ substitute "a" "b"
            , substitute "a" "b"
            , substitute "a" "b"
            ]
        )

    it "#3" do
      1 `shouldEqual`
        ( typeVarPenalty $ l
            [ substitute "a" "b"
            , substitute "a" "c"
            ]
        )

    it "#4" do
      1 `shouldEqual`
        ( typeVarPenalty $ l
            [ substitute "a" "b"
            , substitute "b" "a"
            , substitute "b" "c"
            ]
        )

    it "#5" do
      0 `shouldEqual`
        ( typeVarPenalty $ l
            [ substitute "a" "b"
            , substitute "b" "c"
            , substitute "c" "a"
            ]
        )

    it "#6" do
      2 `shouldEqual`
        ( typeVarPenalty $ l
            [ substitute "a" "b"
            , substitute "a" "c"
            , substitute "a" "a"
            ]
        )

    it "#7" do
      2 `shouldEqual`
        ( typeVarPenalty $ l
            [ substitute "a" "a"
            , substitute "b" "a"
            , substitute "c" "a"
            ]
        )

    it "#8" do
      4 `shouldEqual`
        ( typeVarPenalty $ l
            [ substitute "a" "a"
            , substitute "b" "a"
            , substitute "c" "a"
            , substitute "a" "b"
            , substitute "a" "c"
            , substitute "a" "a"
            ]
        )

    it "#9" do
      0 `shouldEqual`
        ( typeVarPenalty $ l
            [ substitute "a" "e"
            , substitute "b" "d"
            , substitute "c" "f"
            ]
        )
  describe "unification" do
    it "instantiation #0" do
      let
        mVarQuery = qVar "m"
        unitConstQuery = qConst "Unit"

      shouldSatisfy
        (penalty unitConstQuery unitType)
        (_ < penalty mVarQuery unitType)

    it "generalization #0" do
      let
        query = qVar "m"
        t1 = tyVar "m"

      shouldSatisfy
        (penalty query unitType)
        (_ > penalty query t1)

tyVar :: String -> Type'
tyVar = TypeVar unit

tyForAll :: String -> Type' -> Type'
tyForAll name inner = ForAll unit TypeVarInvisible name Nothing inner Nothing

l :: forall f. Foldable f => (forall a. f a -> List a)
l = List.fromFoldable

nl
  :: forall t
   . Foldable t
  => Functor t
  => String
  -> t String
  -> NonEmptyList Identifier
nl x rst = NonEmptyList.cons' (Identifier x) $ List.fromFoldable (rst <#> Identifier)

unitType :: Type'
unitType = TypeConstructor unit
  ( qname [] "Unit"
  )

countFreeVars :: TypeQuery -> Int
countFreeVars = getFreeVariables >>> Set.size

qname :: forall tag. Array String -> String -> Qualified (ProperName tag)
qname m n = Qualified by $ (ProperName n)
  where
  by = ByModuleName $ ModuleName $ String.joinWith "." m

constraint :: Qualified (ProperName ClassName) -> Array Type' -> Constraint'
constraint c a = Constraint
  { ann: unit
  , args: a
  , class: c
  , data: Nothing
  , kindArgs: []
  }

qVar :: String -> TypeQuery
qVar = QVar <<< Identifier

qConst :: String -> TypeQuery
qConst = QConst <<< Identifier

qConstraint :: String -> List TypeQuery -> TypeQuery -> TypeQuery
qConstraint = QConstraint <<< Identifier

substitute :: String -> String -> Substitution
substitute a b = Substitute (Identifier a) (Identifier b)
