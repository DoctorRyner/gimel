{ name = "gimel-examples"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "console"
  , "css"
  , "effect"
  , "filterable"
  , "generics-rep"
  , "gimel-router"
  , "js-timers"
  , "psci-support"
  , "react"
  , "react-dom"
  , "web-html"
  ]
, sources = [ "src/**/*.purs", "../src/**/*.purs" ]
, packages = ../packages.dhall
}
