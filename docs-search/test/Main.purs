module Test.Main where

import Data.Argonaut.Decode
import Data.Argonaut.Parser
import Data.Either
import Data.Maybe
import Effect.Aff
import Partial.Unsafe
import Prelude
import Spago.Search.TypeParser

import Effect (Effect)
import Effect.Console (log)
import Test.Unit (suite, test, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  let mkJson x = unsafePartial $  fromRight $ jsonParser x
  suite "Kind parser" do

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


  suite "Constraint parser" do
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

  suite "Type parser" do
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
        BinaryNoParens
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

assertRight :: forall a. Show a => Eq a => Either String a -> a -> Aff Unit
assertRight eiActual expected =
  case eiActual of
    Left string -> do
      Assert.equal (Right expected) eiActual
    Right actual -> do
      Assert.equal (Right expected) eiActual
