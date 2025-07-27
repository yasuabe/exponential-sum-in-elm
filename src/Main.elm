module Main exposing (..)

import Browser
import Model exposing (Model)
import Update exposing (Msg(..), init, update)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
