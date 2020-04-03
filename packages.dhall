let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200309/packages.dhall sha256:9221987b4e7ea99ccd0efbe056f7bebc872cd92e0058efe5baa181d73359e7b3

let overrides = {=}

let additions =
    { react-mui =
        { dependencies = [ "tscompat" ]
        , repo         = "https://github.com/doolse/purescript-react-mui"
        , version      = "v3.9.313"
        }
    , tscompat = 
        { dependencies = [ "prelude" ]
        , repo         = "https://github.com/doolse/purescript-tscompat"
        , version      = "v1.0.1"
        }
    }

in  upstream // overrides // additions
