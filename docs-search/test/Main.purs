module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Unit (suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Spago.Search.TypeParser
import Data.Argonaut.Decode
import Data.Either
import Data.Maybe
import Data.Argonaut.Parser
import Partial.Unsafe
import Effect.Aff

main :: Effect Unit
main = runTest do
  let mkJson x = unsafePartial $  fromRight $ jsonParser x
  suite "TypeParser" do

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


assertRight :: forall a. Show a => Eq a => Either String a -> a -> Aff Unit
assertRight eiActual expected =
  case eiActual of
    Left string -> do
      Assert.equal eiActual (Right expected)
    Right actual -> do
      Assert.equal eiActual (Right expected)
