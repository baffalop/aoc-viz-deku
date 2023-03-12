{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "deku"
  , "effect"
  , "foldable-traversable"
  , "hyrule"
  , "prelude"
  , "qualified-do"
  , "web-html"
  ]
, packages = ./packages.dhall
}
