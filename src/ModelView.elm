module ModelView exposing (modelView)

import Html as H
import Html exposing (Html, Attribute)
import Html.Attributes as A
import Html.Events exposing (onClick)
import Model exposing (..)


type Cell
    = Empty
    | Slider String


modelView : Model -> Html Command
modelView =
    linesView


linesView : Model -> Html Command
linesView model =
    H.div []
        (List.concat
            [ case model.prevLines of
                [] ->
                    []

                pl :: pls ->
                    List.reverse (lineView True False False pl :: List.map (lineView False False False) pls)
            , [ lineView False False True model.currentLine ]
            , case model.nextLines of
                [] ->
                    []

                sl :: sls ->
                    lineView False True False sl :: List.map (lineView False False False) sls
            ]
        )


lineView : Bool -> Bool -> Bool -> Line -> Html Command
lineView isUp isDown isCurrent line =
    let
        prevOp =
            if isCurrent then
                Just SlideLeft
            else
                Nothing

        nextOp =
            if isCurrent then
                Just SlideRight
            else if isUp then
                Just SlideUp
            else if isDown then
                Just SlideDown
            else
                Nothing
    in
        H.div [ A.style [ ( "clear", "both" ) ] ]
            (List.concat
                [ case line.prevCells of
                    [] ->
                        []

                    p :: ps ->
                        List.reverse (cellView prevOp (Slider p) :: List.map (Slider >> cellView Nothing) ps)
                , if isCurrent then
                    [ cellView Nothing Empty ]
                  else
                    []
                , case line.nextCells of
                    [] ->
                        []

                    s :: ss ->
                        cellView nextOp (Slider s) :: List.map (Slider >> cellView Nothing) ss
                ]
            )


cellView : Maybe Command -> Cell -> Html Command
cellView msg cell =
    case cell of
        Empty ->
            H.div (cellStyle False Nothing) []

        Slider value ->
            H.div (cellStyle True msg)
                [ H.text value ]


cellStyle : Bool -> Maybe Command -> List (Attribute Command)
cellStyle isSlider msg =
    if isSlider then
        sliderStyle msg
    else
        [ holeStyle ]


sliderStyle : Maybe Command -> List (Attribute Command)
sliderStyle msg =
    (A.style
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
    )
        :: case msg of
            Just act ->
                [ onClick act
                , A.style [ ( "cursor", "move" ) ]
                ]

            Nothing ->
                []


holeStyle : Attribute Command
holeStyle =
    A.style
        [ ( "backgroundColor", "gray" )
        , ( "height", "90px" )
        , ( "width", "90px" )
        , ( "margin", "5px" )
        , ( "float", "left" )
        ]
