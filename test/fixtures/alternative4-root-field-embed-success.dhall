{ config =
    let __embed = ./spago.dhall

    in  __embed
      with dependencies = __embed.dependencies # [ "newtype" ]
}.config
