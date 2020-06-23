module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Effect (Effect)
import Gimel.Engine (run)
import Gimel.Html (Html, p, span, text)
import Gimel.Http (post)
import Gimel.Sub (Sub, execEvent)
import Gimel.Types (Update)
import Gimel.Utils (withCmd)

type User =
  { uid :: String
  , fullName :: String
  , role :: String
  }

data Event
  = NoEvent
  | UsersAsk
  | UsersSet (Array User)

type Model =
  { users :: Array User
  }

init :: Model
init =
  { users: []
  }

view :: Model -> Html Event
view model = fold $ map userToHtml model.users

userToHtml :: User -> Html Event
userToHtml user = span []
  [ p [] [text $ "Name: " <> user.fullName]
  , p [] [text $ "Role: " <> user.role]
  ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  NoEvent        -> pure model
  UsersSet users -> pure model {users = users}
  UsersAsk       -> model `withCmd` post "http://localhost:1234/api/users"
                                         {action: "Read", payload: {}}
                                         UsersSet

subs :: Array (Sub Model Event)
subs = [execEvent UsersAsk]

main :: Effect Unit
main = run { init, view, update, subs }
