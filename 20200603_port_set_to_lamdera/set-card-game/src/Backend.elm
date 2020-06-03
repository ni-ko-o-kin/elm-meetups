module Backend exposing (app)

import Lamdera

app =
    Lamdera.Backend.application
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }