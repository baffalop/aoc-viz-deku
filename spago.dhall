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
  , "heterogeneous"
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
  , "web-cssom-view"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
}
