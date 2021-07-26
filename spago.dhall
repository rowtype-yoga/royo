{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut-codecs"
  , "argonaut-core"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "http-methods"
  , "interpolate"
  , "maybe"
  , "media-types"
  , "milkis"
  , "node-process"
  , "prelude"
  , "psci-support"
  , "string-parsers"
  , "strings"
  , "stringutils"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
