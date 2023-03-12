{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "deku"
  , "effect"
  , "foldable-traversable"
  , "hyrule"
  , "integers"
  , "interpolate"
  , "prelude"
  , "qualified-do"
  , "tuples"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
}
