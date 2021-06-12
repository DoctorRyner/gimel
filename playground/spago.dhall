{ name = "gimel-examples"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "css"
  , "effect"
  , "foldable-traversable"
  , "gimel-router"
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
, sources = [ "src/**/*.purs", "../src/**/*.purs" ]
, packages = ../packages.dhall
}
