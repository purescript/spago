( let __embed = { config = ./spago.dhall }

  in  __embed
    with config.dependencies = __embed.config.dependencies # [ "newtype" ]
).config
