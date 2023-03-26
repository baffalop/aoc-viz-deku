{ sources = [ "./src/**/*.purs" ]
, name = "aoc-viz-deku"
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
  , "web-cssom"
  , "web-events"
  , "web-uievents"
  ]
, packages = ./packages.dhall
}
