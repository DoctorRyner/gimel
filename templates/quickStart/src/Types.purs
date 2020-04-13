module Types where

type Model =
    { text :: String
    , number :: Int
    }

data Event
    = NoEvent
    | IncN
    | DecN
    | Print String
    | Combine (Array Event)