module Test.Main where

import Prelude

import Docs.Search.TypeDecoder (Constraint(..), FunDep(..), FunDeps(..), Kind(..), QualifiedName(..), Type(..))
import Docs.Search.Types (Identifier(..))
import Test.TypeQuery as TypeQuery
import Test.IndexBuilder as IndexBuilder
import Test.Declarations as Declarations
import Test.ModuleIndex as ModuleIndex
import Test.UI as UI

import Test.Extra (assertRight)

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest mainTest
  UI.main

mainTest :: TestSuite
mainTest = do
  TypeQuery.tests
  IndexBuilder.tests
  Declarations.tests
  ModuleIndex.tests

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
        (qualified ["Prim"] "Type")

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
        (NamedKind $ qualified ["Prim"] "Type")

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

      assertRight (decodeJson row) (Row $ NamedKind $ qualified ["Prim"] "Type")

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
        (FunKind (Row $ NamedKind $ qualified ["Prim"] "Type")
                 (Row $ NamedKind $ qualified ["Prim"] "Type")
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
        (Constraint { constraintClass: qualified ["Prim"] "Partial"
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
          (TypeConstructor (qualified [ "Control"
                                      , "Monad"
                                      , "ST"
                                      , "Internal"
                                      ]
                            "ST"
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
        TypeOp $ qualified [ "Data", "NaturalTransformation" ] "~>"

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
        (TypeOp $ qualified ["Data", "NaturalTransformation"] "~>")
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
          (TypeConstructor (qualified [ "Data", "Maybe" ] "Maybe"))
          (TypeConstructor (qualified [ "Prim" ] "String"))
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
          (Identifier "tail")
          (TypeApp (TypeConstructor $ qualified [ "Data", "Symbol" ] "SProxy")
                   (TypeVar "t"))
          REmpty

    test "ForAll #1" do
      let forallJson = mkJson """
      {"annotation":[],"tag":"ForAll","contents":["a",{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[],"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"annotation":[],"tag":"TypeVar","contents":"a"}]},null]}
      """
      assertRight (decodeJson forallJson) $
        ForAll "a" Nothing (TypeApp (TypeApp (TypeConstructor $ qualified ["Prim"] "Function")
                                     (TypeConstructor $ qualified ["Prim"] "String"))
                            (TypeVar "a"))

    test "ForAll #2" do
      let forallJson = mkJson """
{
  "annotation": [],
  "tag": "ForAll",
  "contents": [
    "f",
    {
      "annotation": [],
      "tag": "FunKind",
      "contents": [
        {
          "annotation": [],
          "tag": "NamedKind",
          "contents": [
            [
              "Prim",
              "RowList"
            ],
            "RowList"
          ]
        },
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
      ]
    },
    {
      "annotation": [],
      "tag": "TypeApp",
      "contents": [
        {
          "annotation": [],
          "tag": "TypeApp",
          "contents": [
            {
              "annotation": [],
              "tag": "TypeConstructor",
              "contents": [
                [
                  "Prim"
                ],
                "Function"
              ]
            },
            {
              "annotation": [],
              "tag": "TypeApp",
              "contents": [
                {
                  "annotation": [],
                  "tag": "TypeVar",
                  "contents": "f"
                },
                {
                  "annotation": [],
                  "tag": "TypeVar",
                  "contents": "l"
                }
              ]
            }
          ]
        },
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
                  "List",
                  "Types"
                ],
                "List"
              ]
            },
            {
              "annotation": [],
              "tag": "ParensInType",
              "contents": {
                "annotation": [],
                "tag": "TypeApp",
                "contents": [
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
                            "Tuple"
                          ],
                          "Tuple"
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
          ]
        }
      ]
    },
    null
  ]
}
      """
      assertRight (decodeJson forallJson) $
        ForAll "f"
        (Just (FunKind (NamedKind (QualifiedName { moduleNameParts: ["Prim","RowList"], name: Identifier "RowList" })) (NamedKind (QualifiedName { moduleNameParts: ["Prim"], name: Identifier "Type" }))))
        (TypeApp (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Prim"], name: Identifier "Function" })) (TypeApp (TypeVar "f") (TypeVar "l"))) (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Data","List","Types"], name: Identifier "List" })) (ParensInType (TypeApp (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Data","Tuple"], name: Identifier "Tuple" })) (TypeConstructor (QualifiedName { moduleNameParts: ["Prim"], name: Identifier "String" }))) (TypeConstructor (QualifiedName { moduleNameParts: ["Prim"], name: Identifier "String" }))))))




  suite "jsons" do

    test "jsons #1" do
      let json = mkJson """
{"annotation":[],"tag":"ForAll","contents":["o",{"annotation":[],"tag":"ForAll","contents":["r",{"annotation":[],"tag":"ForAll","contents":["l",{"annotation":[],"tag":"ConstrainedType","contents":[{"constraintAnn":[],"constraintClass":[["Type","Data","Boolean"],"And"],"constraintArgs":[{"annotation":[],"tag":"TypeVar","contents":"l"},{"annotation":[],"tag":"TypeVar","contents":"r"},{"annotation":[],"tag":"TypeVar","contents":"o"}],"constraintData":null},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Type","Data","Boolean"],"BProxy"]},{"annotation":[],"tag":"TypeVar","contents":"l"}]}]},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Type","Data","Boolean"],"BProxy"]},{"annotation":[],"tag":"TypeVar","contents":"r"}]}]},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Type","Data","Boolean"],"BProxy"]},{"annotation":[],"tag":"TypeVar","contents":"o"}]}]}]}]},null]},null]},null]}
      """

      assertRight (decodeJson json) $ (ForAll "o" Nothing (ForAll "r" Nothing (ForAll "l" Nothing (ConstrainedType (Constraint { constraintArgs: [(TypeVar "l"),(TypeVar "r"),(TypeVar "o")], constraintClass: (QualifiedName { moduleNameParts: ["Type","Data","Boolean"], name: Identifier "And" }) }) (TypeApp (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Prim"], name: Identifier "Function" })) (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Type","Data","Boolean"], name: Identifier "BProxy" })) (TypeVar "l"))) (TypeApp (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Prim"], name: Identifier "Function" })) (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Type","Data","Boolean"], name: Identifier "BProxy" })) (TypeVar "r"))) (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Type","Data","Boolean"], name: Identifier "BProxy" })) (TypeVar "o"))))))))

  suite "Kind encoder" do
    test "FunKind" do
      let k1 =
            FunKind (Row (NamedKind $ qualified [] "a"))
                    (FunKind (NamedKind $ qualified [] "b")
                             (NamedKind $ qualified [] "b"))
      assertRight (decodeJson $ encodeJson $ k1) k1


qualified :: Array String -> String -> QualifiedName
qualified moduleNameParts name = QualifiedName { moduleNameParts, name: Identifier name }
