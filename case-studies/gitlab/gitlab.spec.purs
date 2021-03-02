module GitLab where

import Quickstrom
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), isJust)

readyWhen = "body[data-qa-selector]"

actions = [ Tuple 1 setPassword
          , Tuple 1 enterLogin
          ]
  where
    setPassword = Sequence [
        Focus "[data-page='passwords:edit'] [name='user[password]']",
        Clear "[data-page='passwords:edit'] [name='user[password]']",
        EnterText "test1234",
        Focus "[data-page='passwords:edit'] [name='user[password_confirmation]']",
        Clear "[data-page='passwords:edit'] [name='user[password_confirmation]']",
        EnterText "test1234",
        Click "input[type=submit]"
      ]
    enterLogin = Sequence [
        Focus "[name='user[password]']",
        Clear "[name='user[password]']",
        EnterText "test1234",
        Focus "input[name='user[login]']",
        Clear "input[name='user[login]']",
        EnterText "root",
        Click "input[type=submit]"
      ]

proposition :: Boolean
proposition =
  let
    initial = true
      -- isJust (queryOne "[data-page='passwords:edit']" {})
      -- || isJust (queryOne "[data-page='sessions:new']" {})
  in
    initial
