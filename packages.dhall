let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210516/packages.dhall sha256:f5e978371d4cdc4b916add9011021509c8d869f4c3f6d0d2694c0e03a85046c8

let overrides = {=}

let additions =
      { gimel-router =
        { dependencies = [ "prelude" ]
        , repo = "https://github.com/DoctorRyner/gimel-router"
        , version = "cc667aa7da6a16826c5c9a476c164eb0c72125ce"
        }
      }

in  upstream // overrides // additions
