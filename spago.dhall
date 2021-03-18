{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "affjax"
  , "argonaut-codecs"
  , "console"
  , "debug"
  , "effect"
  , "interpolate"
  , "milkis"
  , "node-process"
  , "psci-support"
  , "string-parsers"
  , "stringutils"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
