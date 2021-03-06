module IconNonsense exposing (..)

import Html.App exposing (program)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Material.Icon as Icon
import Http
import Json.Decode exposing (array, string)
import Task
import Random exposing (Generator)
import Html.Attributes exposing (style)
import Random.Array
import Array exposing (Array)


type alias Model =
    { iconNames : Array String
    , questionInfo : Maybe QuestionInfo
    , mode : Mode
    }


type Mode
    = ChooseIcon
    | ChooseName


getIconNames : Cmd Msg
getIconNames =
    Http.get (array string) "icon-names.json"
        |> Task.perform (always (RecieveIconName fallbackIconNames)) RecieveIconName


init : ( Model, Cmd Msg )
init =
    { iconNames = Array.fromList [], questionInfo = Nothing, mode = ChooseIcon } ! [ getIconNames ]


type Msg
    = RecieveIconName (Array String)
    | GetNextQuestionInfo Mode
    | SetQuestionInfo QuestionInfo
    | IncorrectSelection


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecieveIconName newNames ->
            { model | iconNames = newNames } ! []

        GetNextQuestionInfo mode ->
            { model | mode = mode } ! [ Random.generate SetQuestionInfo (getQuestionInfoGenerator model.iconNames) ]

        SetQuestionInfo info ->
            { model | questionInfo = Just info } ! []

        IncorrectSelection ->
            model ! []


type alias QuestionInfo =
    --this represntation means there are no invalid states
    { first : String
    , second : String
    , third : String
    , correctEntry : QuestionEntry
    }


type QuestionEntry
    = First
    | Second
    | Third


getCorrectString : QuestionInfo -> String
getCorrectString info =
    case info.correctEntry of
        First ->
            info.first

        Second ->
            info.second

        Third ->
            info.third


chooseNext =
    (\( priorChoice, currentDeck ) ->
        Random.Array.choose currentDeck
            |> Random.map (\( choice, rest ) -> ( ( priorChoice, choice ), rest ))
    )


questionEntryGenerator =
    Random.int 0 2
        |> Random.map intToQuestionEntry


intToQuestionEntry n =
    case n of
        0 ->
            First

        1 ->
            Second

        _ ->
            Third


getQuestionInfoGenerator : Array String -> Generator QuestionInfo
getQuestionInfoGenerator deck =
    Random.Array.choose deck
        `Random.andThen` chooseNext
        `Random.andThen` chooseNext
        |> Random.map fst
        |> Random.map
            (\tuple ->
                case tuple of
                    ( ( Just first, Just second ), Just third ) ->
                        QuestionInfo first second third

                    _ ->
                        QuestionInfo "change_history" "donut_large" "donut_small"
            )
        |> Random.map2 (|>) questionEntryGenerator


modeButtons =
    [ Html.button [ onClick <| GetNextQuestionInfo ChooseIcon ] [ text "Choose Icon Mode" ]
    , Html.button [ onClick <| GetNextQuestionInfo ChooseName ] [ text "Choose Name Mode" ]
    ]


view : Model -> Html Msg
view model =
    div [ style [ ( "display", "flex" ), ( "flex-direction", "column" ), ( "align-items", "center" ) ] ]
        <| case model.questionInfo of
            Just info ->
                (case model.mode of
                    ChooseName ->
                        [ text "Pick the name they gave to this icon:"
                        , info |> getCorrectString |> viewIcon
                        , getListOfButtons model.mode info
                        ]

                    ChooseIcon ->
                        [ text "Pick the icon they gave this name to:"
                        , [ info |> getCorrectString |> text ] |> Html.strong []
                        , getListOfButtons model.mode info
                        ]
                )
                    ++ [ div [ style [ ( "margin-top", "5VH" ) ] ] modeButtons ]

            Nothing ->
                modeButtons


getListOfButtons : Mode -> QuestionInfo -> Html Msg
getListOfButtons mode info =
    let
        viewFunction =
            case mode of
                ChooseName ->
                    text

                ChooseIcon ->
                    viewIcon
    in
        div [ style [ ( "display", "flex" ), ( "flex-direction", "row" ) ] ]
            [ Html.button
                [ onClick
                    <| if info.correctEntry == First then
                        GetNextQuestionInfo mode
                       else
                        IncorrectSelection
                ]
                [ viewFunction info.first ]
            , Html.button
                [ onClick
                    <| if info.correctEntry == Second then
                        GetNextQuestionInfo mode
                       else
                        IncorrectSelection
                ]
                [ viewFunction info.second ]
            , Html.button
                [ onClick
                    <| if info.correctEntry == Third then
                        GetNextQuestionInfo mode
                       else
                        IncorrectSelection
                ]
                [ viewFunction info.third ]
            ]


viewIcon : String -> Html Msg
viewIcon name =
    Icon.view name [ Icon.size48 ]


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
    Array.fromList [ "change_history", "donut_large", "donut_small", "flip_to_back", "grade", "perm_data_setting" ]
