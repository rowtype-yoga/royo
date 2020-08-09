{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "console"
  , "debug"
  , "effect"
  , "interpolate"
  , "milkis"
  , "node-process"
  , "psci-support"
  , "simple-json"
  , "string-parsers"
  , "stringutils"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
