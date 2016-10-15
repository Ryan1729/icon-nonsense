module IconNonsense exposing (..)

import Html.App exposing (program)
import Html exposing (Html, div)
import Material.Icon as Icon
import Http
import Json.Decode exposing (list, string)
import Task


type alias Model =
    { iconNames : List String }


getIconNames : Cmd Msg
getIconNames =
    Http.get (list string) "icon-names.json"
        |> Task.perform (always (RecieveIconName fallbackIconNames)) RecieveIconName


init : ( Model, Cmd Msg )
init =
    { iconNames = [] } ! [ getIconNames ]


type Msg
    = RecieveIconName (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecieveIconName newNames ->
            { model | iconNames = newNames } ! []


view : Model -> Html Msg
view model =
    model.iconNames
        |> List.map Icon.i
        |> div []


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


fallbackIconNames =
    [ "change_history", "donut_large", "donut_small", "flip_to_back", "grade", "perm_data_setting" ]
