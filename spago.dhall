{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "arrays"
  , "control"
  , "debug"
  , "deku"
  , "effect"
  , "filterable"
  , "foldable-traversable"
  , "hyrule"
  , "integers"
  , "interpolate"
  , "maybe"
  , "numbers"
  , "point-free"
  , "prelude"
  , "qualified-do"
  , "tuples"
  , "web-events"
  ]
, packages = ./packages.dhall
}
