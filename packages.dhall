let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200309/packages.dhall sha256:9221987b4e7ea99ccd0efbe056f7bebc872cd92e0058efe5baa181d73359e7b3

let overrides = {=}

let additions =
    { gimel-router =
        { dependencies = [ "prelude" ]
        , repo         = "https://github.com/DoctorRyner/gimel-router"
        , version      = "cc667aa7da6a16826c5c9a476c164eb0c72125ce"
        }
    }

in  upstream // overrides // additions
