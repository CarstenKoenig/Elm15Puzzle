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


moveUp : Model -> Model
moveUp model =
    case model.prevLines of
        [] ->
            model

        p :: ps ->
            let
                ( p2, v ) =
                    pop p
            in
                { prevLines = ps
                , currentLine = p2
                , nextLines = push v model.currentLine :: model.nextLines
                }


moveDown : Model -> Model
moveDown model =
    case model.nextLines of
        [] ->
            model

        n :: ns ->
            let
                ( n2, v ) =
                    pop n
            in
                { prevLines = push v model.currentLine :: model.prevLines
                , currentLine = n2
                , nextLines = ns
                }


moveLeft : Model -> Model
moveLeft model =
    { prevLines = List.map moveLineLeft model.prevLines
    , currentLine = moveLineLeft model.currentLine
    , nextLines = List.map moveLineLeft model.nextLines
    }


moveRight : Model -> Model
moveRight model =
    { prevLines = List.map moveLineRight model.prevLines
    , currentLine = moveLineRight model.currentLine
    , nextLines = List.map moveLineRight model.nextLines
    }


moveLineLeft : Line -> Line
moveLineLeft line =
    case line.prevCells of
        [] ->
            line

        p :: ps ->
            { prevCells = ps
            , nextCells = (p :: line.nextCells)
            }


moveLineRight : Line -> Line
moveLineRight line =
    case line.nextCells of
        [] ->
            line

        n :: ns ->
            { prevCells = (n :: line.prevCells)
            , nextCells = ns
            }


pop : Line -> ( Line, String )
pop line =
    case line.nextCells of
        s :: ss ->
            ( { line | nextCells = ss }, s )

        [] ->
            ( line, "fail" )


push : String -> Line -> Line
push s line =
    { line | nextCells = s :: line.nextCells }


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

        SlideLeft ->
            moveLeft model

        SlideRight ->
            moveRight model

        SlideUp ->
            moveUp model

        SlideDown ->
            moveDown model
