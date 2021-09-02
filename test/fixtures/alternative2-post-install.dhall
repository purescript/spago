    ( let __embed = ./spago.dhall

      in  __embed
        with dependencies = __embed.dependencies # [ "simple-json" ]
    )
//  { sources = [ "src/**/*.purs" ] }
