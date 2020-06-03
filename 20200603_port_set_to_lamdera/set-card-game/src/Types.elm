module Types exposing (..)

import Backend
import Frontend


type alias FrontendModel =
    Frontend.Model


type alias BackendModel =
    Backend.Model


type alias FrontendMsg =
    Frontend.Msg


type ToBackend
    = NoOpToBackend


type alias BackendMsg =
    Backend.Msg


type ToFrontend
    = NoOpToFrontend
