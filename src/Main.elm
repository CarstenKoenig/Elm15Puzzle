module Main exposing (..)

import Html as H
import Html exposing (Html, Attribute, beginnerProgram)
import Html.Attributes as A
import Html.Events exposing (onClick)


type Cell
    = Empty
    | Slider String


type alias Board =
    List (List Cell)


type alias Model =
    { prevLines : List Line
    , currentLine : Line
    , nextLines : List Line
    }


type alias Line =
    { prevCells : List String
    , nextCells : List String
    }


modelToBoard : Model -> Board
modelToBoard model =
    List.concat
        [ List.map (lineToBoard False) (List.reverse model.prevLines)
        , [ lineToBoard True model.currentLine ]
        , List.map (lineToBoard False) model.nextLines
        ]


lineToBoard : Bool -> Line -> List Cell
lineToBoard isCurrent line =
    List.concat
        [ List.map Slider (List.reverse line.prevCells)
        , if isCurrent then
            [ Empty ]
          else
            []
        , List.map Slider line.nextCells
        ]


solvedModel : Model
solvedModel =
    let
        cells f t =
            List.reverse (List.map toString (List.range f t))

        line f t =
            { prevCells = cells f (t - 1)
            , nextCells = [ toString t ]
            }

        prevLs =
            [ line 9 12
            , line 5 8
            , line 1 4
            ]

        prevCs =
            cells 13 15

        curL =
            { prevCells = prevCs
            , nextCells = []
            }
    in
        { prevLines = prevLs
        , currentLine = curL
        , nextLines = []
        }


main =
    beginnerProgram
        { model = solvedModel
        , view = modelView
        , update = update
        }


modelView : Model -> Html Msg
modelView =
    linesView


linesView : Model -> Html Msg
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


lineView : Bool -> Bool -> Bool -> Line -> Html Msg
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


cellView : Maybe Msg -> Cell -> Html Msg
cellView msg cell =
    case cell of
        Empty ->
            H.div (cellStyle False Nothing) []

        Slider value ->
            H.div (cellStyle True msg)
                [ H.text value ]


cellStyle : Bool -> Maybe Msg -> List (Attribute Msg)
cellStyle isSlider msg =
    if isSlider then
        sliderStyle msg
    else
        [ holeStyle ]


sliderStyle : Maybe Msg -> List (Attribute Msg)
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
    | SlideLeft
    | SlideRight
    | SlideUp
    | SlideDown


update msg model =
    case msg of
        NoOp ->
            model

        _ ->
            Debug.log (toString msg) model
