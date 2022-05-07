module Test.TypeJson
       ( tests )
where

import Prelude

import Docs.Search.TypeDecoder (Constraint(..), FunDep(..), FunDeps(..), QualifiedName(..), Type(..))
import Docs.Search.Types (Identifier(..))
import Test.Extra (assertRight)
import Test.TypeQuery as TypeQuery

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, suite, test)


tests :: TestSuite
tests = do
  suite "FunDeps decoder" do
    test "FunDeps" do
      let
        funDeps = mkJson """
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

    test "KindApp" do
      let kindAppJson = mkJson """
        {"annotation":[],"tag":"KindApp","contents":[{"annotation":[],"tag":"REmpty"},{"annotation":[],"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]}
      """
      assertRight (decodeJson kindAppJson) $
        KindApp REmpty (TypeConstructor (qualified ["Prim"] "Type"))
  suite "jsons" do

    test "jsons #1" do
      let json = mkJson """
{"annotation":[],"tag":"ForAll","contents":["o",{"annotation":[],"tag":"ForAll","contents":["r",{"annotation":[],"tag":"ForAll","contents":["l",{"annotation":[],"tag":"ConstrainedType","contents":[{"constraintAnn":[],"constraintClass":[["Type","Data","Boolean"],"And"],"constraintArgs":[{"annotation":[],"tag":"TypeVar","contents":"l"},{"annotation":[],"tag":"TypeVar","contents":"r"},{"annotation":[],"tag":"TypeVar","contents":"o"}],"constraintData":null},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Type","Data","Boolean"],"BProxy"]},{"annotation":[],"tag":"TypeVar","contents":"l"}]}]},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Type","Data","Boolean"],"BProxy"]},{"annotation":[],"tag":"TypeVar","contents":"r"}]}]},{"annotation":[],"tag":"TypeApp","contents":[{"annotation":[],"tag":"TypeConstructor","contents":[["Type","Data","Boolean"],"BProxy"]},{"annotation":[],"tag":"TypeVar","contents":"o"}]}]}]}]},null]},null]},null]}
      """

      assertRight (decodeJson json) $ (ForAll "o" Nothing (ForAll "r" Nothing (ForAll "l" Nothing (ConstrainedType (Constraint { constraintArgs: [(TypeVar "l"),(TypeVar "r"),(TypeVar "o")], constraintClass: (QualifiedName { moduleNameParts: ["Type","Data","Boolean"], name: Identifier "And" }) }) (TypeApp (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Prim"], name: Identifier "Function" })) (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Type","Data","Boolean"], name: Identifier "BProxy" })) (TypeVar "l"))) (TypeApp (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Prim"], name: Identifier "Function" })) (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Type","Data","Boolean"], name: Identifier "BProxy" })) (TypeVar "r"))) (TypeApp (TypeConstructor (QualifiedName { moduleNameParts: ["Type","Data","Boolean"], name: Identifier "BProxy" })) (TypeVar "o"))))))))


qualified :: Array String -> String -> QualifiedName
qualified = TypeQuery.qname

mkJson :: String -> Json
mkJson str =
  unsafePartial $ case jsonParser str of
    Right r -> r
