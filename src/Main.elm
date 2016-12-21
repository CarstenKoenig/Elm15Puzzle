module Main exposing (..)

import Html as H
import Html exposing (Html, Attribute, beginnerProgram)
import Model exposing (..)
import ModelView exposing (modelView)


main : Program Never Model Msg
main =
    beginnerProgram
        { model = solvedModel
        , view = modelView >> H.map Slider
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Slider cmd ->
            updateModel cmd model


type Msg
    = Slider Command
    | NoOp
