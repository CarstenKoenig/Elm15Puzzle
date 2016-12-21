module Main exposing (..)

import Html as H
import Html exposing (Html, Attribute, beginnerProgram)
import Html.Attributes as A
import Html.Events exposing (onClick)


type alias Cell =
    { value : Maybe String }


type alias Board =
    List (List Cell)


solvedBoard : Board
solvedBoard =
    [ List.map (\i -> { value = Just (toString i) }) (List.range 1 4)
    , List.map (\i -> { value = Just (toString i) }) (List.range 5 8)
    , List.map (\i -> { value = Just (toString i) }) (List.range 9 12)
    , List.map (\i -> { value = Just (toString i) }) (List.range 13 15) ++ [ { value = Nothing } ]
    ]


main =
    beginnerProgram
        { model = solvedBoard
        , view = boardView
        , update = update
        }


boardView : Board -> Html Msg
boardView board =
    H.div []
        (List.map rowView board)


rowView : List Cell -> Html Msg
rowView cells =
    H.div [ A.style [ ( "clear", "both" ) ] ]
        (List.map cellView cells)


cellView : Cell -> Html Msg
cellView cell =
    case cell.value of
        Nothing ->
            H.div [ cellStyle False ] []

        Just value ->
            H.div [ cellStyle True ]
                [ H.text value ]


cellStyle : Bool -> Attribute Msg
cellStyle isSlider =
    if isSlider then
        sliderStyle
    else
        holeStyle


sliderStyle : Attribute Msg
sliderStyle =
    A.style
        [ ( "backgroundColor", "slateblue" )
        , ( "height", "90px" )
        , ( "width", "90px" )
        , ( "margin", "5px" )
        , ( "text-align", "center" )
        , ( "vertical-align", "middle" )
        , ( "line-height", "90px" )
        , ( "font-weigth", "bold" )
        , ( "font-size", "2em" )
        , ( "float", "left" )
        ]


holeStyle : Attribute Msg
holeStyle =
    A.style
        [ ( "backgroundColor", "gray" )
        , ( "height", "90px" )
        , ( "width", "90px" )
        , ( "margin", "5px" )
        , ( "float", "left" )
        ]


type Msg
    = NoOp


update msg model =
    case msg of
        NoOp ->
            model
