module Backend exposing (..)

import Lamdera


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { message : String
    }


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    ( { message = "Hello" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


updateFromFrontend : Lamdera.SessionId -> Lamdera.ClientId -> toBackend -> Model -> ( Model, Cmd Msg )
updateFromFrontend session_id clientId toBackend model =
    ( model, Cmd.none )
