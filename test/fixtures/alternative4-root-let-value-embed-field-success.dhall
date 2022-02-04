let x =
      { key1 =
          let __embed = ./spago.dhall

          in  __embed
            with dependencies = __embed.dependencies # [ "newtype" ]
      }

in  x.key1
