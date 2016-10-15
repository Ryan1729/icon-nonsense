module IconNonsense exposing (..)

import Html.App exposing (program)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Material.Icon as Icon
import Http
import Json.Decode exposing (list, string)
import Task
import Random exposing (Generator)
import Html.Attributes exposing (style)


type alias Model =
    { iconNames : List String
    , questionInfo : Maybe QuestionInfo
    }


getIconNames : Cmd Msg
getIconNames =
    Http.get (list string) "icon-names.json"
        |> Task.perform (always (RecieveIconName fallbackIconNames)) RecieveIconName


init : ( Model, Cmd Msg )
init =
    { iconNames = [], questionInfo = Nothing } ! [ getIconNames ]


type Msg
    = RecieveIconName (List String)
    | GetNextQuestionInfo
    | SetQuestionInfo QuestionInfo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecieveIconName newNames ->
            { model | iconNames = newNames } ! []

        GetNextQuestionInfo ->
            model ! [ Random.generate SetQuestionInfo questionInfoGenerator ]

        SetQuestionInfo info ->
            { model | questionInfo = Just info } ! []


type alias QuestionInfo =
    String


questionInfoGenerator : Generator QuestionInfo
questionInfoGenerator =
    Random.int 0 6
        |> Random.map (\index -> getAt index fallbackIconNames |> Maybe.withDefault "pan_tool")


view : Model -> Html Msg
view model =
    case model.questionInfo of
        Just info ->
            div [ style [ ( "display", "flex" ), ( "flex-direction", "column" ), ( "align-items", "center" ) ] ]
                [ text <| toString info
                , viewIcon info
                , div [ style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
                    [ Html.button [ onClick GetNextQuestionInfo ] [ text info ]
                    , Html.button [ onClick GetNextQuestionInfo ] [ text info ]
                    , Html.button [ onClick GetNextQuestionInfo ] [ text info ]
                    ]
                ]

        Nothing ->
            Html.button [ onClick GetNextQuestionInfo ] [ text "Start" ]


viewIcon : String -> Html Msg
viewIcon name =
    Icon.view name [ Icon.size48 ]


getQuestionInfo : List String -> QuestionInfo
getQuestionInfo list =
    "TODO"


getAt : Int -> List a -> Maybe a
getAt i list =
    list
        |> List.drop i
        |> List.head


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
