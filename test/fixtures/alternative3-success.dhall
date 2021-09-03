let deps = [] : List Text

let x =
          ( ./spago.dhall
            with dependencies =
                (./spago.dhall).dependencies # deps # [ "newtype" ]
            with sources = (./spago.dhall).sources # ([] : List Text)
          )
      //  { subConfig.name = "foo" }.subConfig

let y = x

in  y
  with sources = y.sources # ([] : List Text)
