let __embed = ./spago.dhall

in      __embed
    //  { dependencies = __embed.dependencies # [ "simple-json" ]
        , sources = [ "src/**/*.purs" ]
        }
