{ name = "gimel-examples"
, dependencies =
  [ "aff"
  , "console"
  , "css"
  , "effect"
  , "js-timers"
  , "psci-support"
  , "react"
  , "react-dom"
  , "web-html"
  ]
, sources = [ "src/**/*.purs", "../src/**/*.purs" ]
, packages = ../packages.dhall
}
