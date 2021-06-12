{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "css"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "js-timers"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "react"
  , "react-dom"
  , "refs"
  , "unsafe-coerce"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
