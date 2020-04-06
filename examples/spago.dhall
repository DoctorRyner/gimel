{ name =
    "gimel-examples"
, dependencies =
    [ "aff"
    , "console"
    , "effect"
    , "psci-support"
    , "react"
    , "react-dom"
    , "web-html"
    ]
, sources =
    [ "src/**/*.purs"
    , "../src/**/*.purs"
    ]
, packages =
    ../packages.dhall
}