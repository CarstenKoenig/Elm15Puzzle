module Model exposing (Model, Line, Command(..), updateModel, solvedModel)


type alias Model =
    { prevLines : List Line
    , currentLine : Line
    , nextLines : List Line
    }


type alias Line =
    { prevCells : List String
    , nextCells : List String
    }


type Command
    = SlideLeft
    | SlideRight
    | SlideUp
    | SlideDown


updateModel : Command -> Model -> Model
updateModel cmd model =
    case cmd of
        SlideLeft ->
            moveLeft model

        SlideRight ->
            moveRight model

        SlideUp ->
            moveUp model

        SlideDown ->
            moveDown model


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
