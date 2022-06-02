{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "secav-unshortener"
, dependencies =
  [ "concur-core"
  , "concur-react"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "tuples"
  ]
, packages = ./packages.dhall
}
