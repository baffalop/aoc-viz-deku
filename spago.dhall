{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "deku"
  , "effect"
  , "filterable"
  , "foldable-traversable"
  , "hyrule"
  , "integers"
  , "interpolate"
  , "maybe"
  , "numbers"
  , "prelude"
  , "qualified-do"
  , "tuples"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
}
