{ name = "my-project"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "filterable"
  , "ordered-collections"
  , "psci-support"
  , "react"
  , "react-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
