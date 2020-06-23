module Main where

import Prelude hiding (div)

import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gimel.Engine (run)
import Gimel.Html (Html, h1, text, textS)
import Gimel.Http (post)
import Gimel.Sub (Sub)
import Gimel.Sub.Time (delay)
import Gimel.Types (Update)

type LoginResult =
  { id :: Int
  , token :: String
  }

data Event = SaveLoginResult LoginResult

type Model =
  { loginResult :: Maybe LoginResult
  }

init :: Model
init =
  { loginResult: Nothing
  }

view :: Model -> Html Event
view model = case model.loginResult of
  Nothing          -> text "Didn't get login result yet! (Wait for 2 seconds)"
  Just loginResult -> fold [ h1 [] [text "Login result:"]
                           , textS loginResult
                           ]

update :: Model -> Event -> Update Model Event
update model = case _ of
  SaveLoginResult loginResult -> pure model {loginResult = Just loginResult}

subs :: Array (Sub Model Event)
subs = 
  [ delay 2.0 $ post "https://reqres.in/api/register"
                     { email: "eve.holt@reqres.in"
                     , password: "pistol"
                     }
                     SaveLoginResult
  ]

main :: Effect Unit
main = run { init, view, update, subs }
