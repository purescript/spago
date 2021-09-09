let x =
      let __embed = { key1 = ./spago.dhall }

      in  __embed
        with key1.dependencies = __embed.key1.dependencies # [ "newtype" ]

in  x.key1
