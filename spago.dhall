{ name = "my-project"
, dependencies =
  [ "aff"
  , "console"
  , "css"
  , "effect"
  , "js-timers"
  , "ordered-collections"
  , "psci-support"
  , "react"
  , "react-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
