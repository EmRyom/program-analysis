{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "secav-unshortener"
, dependencies =
  [ "arrays"
  , "concur-core"
  , "concur-react"
  , "console"
  , "effect"
  , "either"
  , "parsing"
  , "stringutils"
  ]
, packages = ./packages.dhall
}
