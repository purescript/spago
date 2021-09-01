let __embed = ./spago.dhall

in      __embed
    //  (     { sources = [ "src/**/*.purs" ] }
          //  { dependencies = __embed.dependencies # [ "simple-json" ] }
        )
