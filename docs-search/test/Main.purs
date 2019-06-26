module Test.Main where

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
import Spago.Search.Declarations
import Spago.Search.TypeParser
import Test.Unit (suite, test, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  let mkJson x = unsafePartial $ fromRight $ jsonParser x
  suite "FunDeps decoder" do
    test "FunDeps" do
      let funDeps = mkJson """
       [
          [
            [
              "lhs",
              "rhs"
            ],
            [
              "output"
            ]
          ]
        ]
      """
      assertRight (decodeJson funDeps)
        (FunDeps [ FunDep { lhs: [ "lhs", "rhs" ]
                          , rhs: [ "output"]
                          }
                 ])

  suite "Kind decoder" do

    test "QualifiedName" do

      let qualifiedName =  mkJson """
      [
          [
            "Prim"
          ],
          "Type"
      ]
      """

      assertRight (decodeJson qualifiedName)
        (QualifiedName { moduleName: ["Prim"]
                       , name: "Type"
                       }
        )

    test "NamedKind" do
      let namedKind = mkJson """
      {
        "annotation": [],
        "tag": "NamedKind",
        "contents": [
          [
            "Prim"
          ],
          "Type"
        ]
      }
      """

      assertRight (decodeJson namedKind)
        (NamedKind $ QualifiedName { moduleName: ["Prim"]
                                   , name: "Type"
                                   }
        )

    test "Row" do
      let row = mkJson """
            {
              "annotation": [],
              "tag": "Row",
              "contents": {
                "annotation": [],
                "tag": "NamedKind",
                "contents": [
                  [
                    "Prim"
                  ],
                  "Type"
                ]
              }
            }
      """

      assertRight (decodeJson row)
        (Row $ NamedKind $ QualifiedName { moduleName: ["Prim"]
                                         , name: "Type"
                                         }
        )

    test "FunKind" do
      let funKind = mkJson """
        {
          "annotation": [],
          "tag": "FunKind",
          "contents": [
            {
              "annotation": [],
              "tag": "Row",
              "contents": {
                "annotation": [],
                "tag": "NamedKind",
                "contents": [
                  [
                    "Prim"
                  ],
                  "Type"
                ]
              }
            },
            {
              "annotation": [],
              "tag": "Row",
              "contents": {
                "annotation": [],
                "tag": "NamedKind",
                "contents": [
                  [
                    "Prim"
                  ],
                  "Type"
                ]
              }
            }
          ]
        }
        """
      assertRight (decodeJson funKind)
        (FunKind (Row $ NamedKind $ QualifiedName { moduleName: ["Prim"]
                                                  , name: "Type"
                                                  }
                 )
                 (Row $ NamedKind $ QualifiedName { moduleName: ["Prim"]
                                                  , name: "Type"
                                                  }
                 )
        )


  suite "Constraint decoder" do
    test "Constraint" do
      let constraint = mkJson """
      {
        "constraintAnn": [],
        "constraintClass": [
          [
            "Prim"
          ],
          "Partial"
        ],
        "constraintArgs": [],
        "constraintData": null
      }
      """
      assertRight (decodeJson constraint)
        (Constraint { constraintClass: QualifiedName { moduleName: ["Prim"]
                                                     , name: "Partial"
                                                     }
                    , constraintArgs: []
                    })

  suite "Type decoder" do
    test "TypeVar" do
      let typeVar = mkJson """
      {
        "annotation": [],
        "tag": "TypeVar",
        "contents": "m"
      }
      """

      assertRight (decodeJson typeVar)
       (TypeVar "m")

    test "TypeApp" do
      let typeApp1 = mkJson """
        {
          "annotation": [],
          "tag": "TypeApp",
          "contents": [
            {
              "annotation": [],
              "tag": "TypeConstructor",
              "contents": [
                [
                  "Control",
                  "Monad",
                  "ST",
                  "Internal"
                ],
                "ST"
              ]
            },
            {
              "annotation": [],
              "tag": "TypeVar",
              "contents": "h"
            }
          ]
        }
      """

      assertRight (decodeJson typeApp1) $
        TypeApp
          (TypeConstructor (QualifiedName { moduleName:
                                            [ "Control"
                                            , "Monad"
                                            , "ST"
                                            , "Internal"
                                            ],
                                            name: "ST"
                                          }
                           ))
          (TypeVar "h")

    test "TypeOp" do
      let typeOp = mkJson """
          {
            "annotation": [],
            "tag": "TypeOp",
            "contents": [
              [
                "Data",
                "NaturalTransformation"
              ],
              "~>"
            ]
          }
      """
      assertRight (decodeJson typeOp) $
        TypeOp $ QualifiedName { moduleName: [ "Data", "NaturalTransformation" ]
                               , name: "~>"
                               }

    test "BinaryNoParens" do
      let binaryNoParens = mkJson """
      {
        "annotation": [],
        "tag": "BinaryNoParensType",
        "contents": [
          {
            "annotation": [],
            "tag": "TypeOp",
            "contents": [
              [
                "Data",
                "NaturalTransformation"
              ],
              "~>"
            ]
          },
          {
            "annotation": [],
            "tag": "TypeVar",
            "contents": "m"
          },
          {
            "annotation": [],
            "tag": "TypeVar",
            "contents": "n"
          }
        ]
      }
      """

      assertRight (decodeJson binaryNoParens) $
        BinaryNoParensType
        (TypeOp $ QualifiedName { moduleName: ["Data", "NaturalTransformation"], name: "~>" })
        (TypeVar "m")
        (TypeVar "n")

    test "ParensInType" do
      let parensInType = mkJson """
        {
          "annotation": [],
          "tag": "ParensInType",
          "contents": {
            "annotation": [],
            "tag": "TypeApp",
            "contents": [
              {
                "annotation": [],
                "tag": "TypeConstructor",
                "contents": [
                  [
                    "Data",
                    "Maybe"
                  ],
                  "Maybe"
                ]
              },
              {
                "annotation": [],
                "tag": "TypeConstructor",
                "contents": [
                  [
                    "Prim"
                  ],
                  "String"
                ]
              }
            ]
          }
        }
      """

      assertRight (decodeJson parensInType) $
        ParensInType $
          TypeApp
          (TypeConstructor (QualifiedName { moduleName:
                                            [ "Data"
                                            , "Maybe"
                                            ],
                                            name: "Maybe"
                                          }
                           ))
          (TypeConstructor (QualifiedName { moduleName:
                                            [ "Prim"
                                            ],
                                            name: "String"
                                          }
                           ))
    test "RCons" do

      let rcons = mkJson """
      {
        "annotation": [],
        "tag": "RCons",
        "contents": [
          "tail",
          {
            "annotation": [],
            "tag": "TypeApp",
            "contents": [
              {
                "annotation": [],
                "tag": "TypeConstructor",
                "contents": [
                  [
                    "Data",
                    "Symbol"
                  ],
                  "SProxy"
                ]
              },
              {
                "annotation": [],
                "tag": "TypeVar",
                "contents": "t"
              }
            ]
          },
          {
            "annotation": [],
            "tag": "REmpty"
          }
        ]
      }
      """

      assertRight (decodeJson rcons) $
        RCons
          "tail"
          (TypeApp (TypeConstructor $ QualifiedName { moduleName: [ "Data", "Symbol" ], name: "SProxy" })
                   (TypeVar "t"))
          REmpty

    test "ForAll" do
      let forallJson = mkJson """
      {"annotation":[],"tag":"ForAll","contents":["a",{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[],"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"annotation":[],"tag":"TypeVar","contents":"a"}]},null]}
      """
      assertRight (decodeJson forallJson) $
        ForAll "a" (TypeApp (TypeApp (TypeConstructor $ qualified ["Prim"] "Function")
                                     (TypeConstructor $ qualified ["Prim"] "String"))
                            (TypeVar "a")) Nothing
  suite "jsons" do

    test "jsons #1" do
      let json = mkJson """
{"annotation":[],"tag":"ForAll","contents":["o",{"annotation":[],"tag":"ForAll","contents":["r",{"annotation":[],"tag":"ForAll","contents":["l",{"annotation":[],"tag":"ConstrainedType","contents":[{"constraintAnn":[],"constraintClass":[["Type","Data","Boolean"],"And"],"constraintArgs":[{"annotation":[],"tag":"TypeVar","contents":"l"},{"annotation":[],"tag":"TypeVar","contents":"r"},{"annotation":[],"tag":"TypeVar","contents":"o"}],"constraintData":null},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Type","Data","Boolean"],"BProxy"]},{"annotation":[],"tag":"TypeVar","contents":"l"}]}]},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Type","Data","Boolean"],"BProxy"]},{"annotation":[],"tag":"TypeVar","contents":"r"}]}]},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Type","Data","Boolean"],"BProxy"]},{"annotation":[],"tag":"TypeVar","contents":"o"}]}]}]}]},null]},null]},null]}
      """

      assertRight (decodeJson json) $ (ForAll "o" (ForAll "r" (ForAll "l" (ConstrainedType (Constraint { constraintArgs: [(TypeVar "l"),(TypeVar "r"),(TypeVar "o")], constraintClass: (QualifiedName { moduleName: ["Type","Data","Boolean"], name: "And" }) }) (TypeApp (TypeApp (TypeConstructor (QualifiedName { moduleName: ["Prim"], name: "Function" })) (TypeApp (TypeConstructor (QualifiedName { moduleName: ["Type","Data","Boolean"], name: "BProxy" })) (TypeVar "l"))) (TypeApp (TypeApp (TypeConstructor (QualifiedName { moduleName: ["Prim"], name: "Function" })) (TypeApp (TypeConstructor (QualifiedName { moduleName: ["Type","Data","Boolean"], name: "BProxy" })) (TypeVar "r"))) (TypeApp (TypeConstructor (QualifiedName { moduleName: ["Type","Data","Boolean"], name: "BProxy" })) (TypeVar "o"))))) Nothing) Nothing) Nothing)

  suite "Kind encoder" do
    test "FunKind" do
      let k1 =
            FunKind (Row (NamedKind $ qualified [] "a"))
                    (FunKind (NamedKind $ qualified [] "b")
                             (NamedKind $ qualified [] "b"))
      assertRight (decodeJson $ encodeJson $ k1) k1


qualified :: Array String -> String -> QualifiedName
qualified moduleName name = QualifiedName { moduleName, name }

assertRight :: forall a. Show a => Eq a => Either String a -> a -> Aff Unit
assertRight eiActual expected =
  case eiActual of
    Left string -> do
      Assert.equal (Right expected) eiActual
    Right actual -> do
      Assert.equal (Right expected) eiActual
