module Backend exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    BackendModel


type alias Msg =
    BackendMsg


init : ( Model, Cmd Msg )
init =
    ( { players = Dict.empty }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


updateFromFrontend : Lamdera.SessionId -> Lamdera.ClientId -> ToBackend -> Model -> ( Model, Cmd Msg )
updateFromFrontend session_id clientId toBackendMsg model =
    case toBackendMsg of
        PlayerJoined name ->
            ( { model | players = Dict.insert clientId name model.players }, Cmd.none )



-- case msg of
--         -- A new client has joined! Add them to our clients list, and send them all messages we have so far.
--         ClientJoin ->
--             let
--                 newModel =
--                     { model | clients = Set.insert clientId model.clients }
--                 sendHelloMessageToAllClients =
--                     broadcast newModel.clients (ClientJoinReceived clientId)
--                 sendMessageHistoryToNewlyJoinedClient =
--                     model.messages
--                         -- |> List.reverse -- Que? Is this a bug?
--                         |> List.map RoomMsgReceived
--                         |> List.map (Lamdera.sendToFrontend clientId)
--                         |> Cmd.batch
--             in
--             ( newModel
--             , Cmd.batch
--                 [ sendHelloMessageToAllClients
--                 , sendMessageHistoryToNewlyJoinedClient
--                 ]
--             )
--         -- A client has sent us a new message! Add it to our messages list, and broadcast it to everyone.
--         MsgSubmitted text ->
--             ( { model | messages = ( clientId, text ) :: model.messages }
--             , broadcast model.clients (RoomMsgReceived ( clientId, text ))
--             )
