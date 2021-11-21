module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Random
import Time


gameHeight : number
gameHeight =
    30


gameWidth : number
gameWidth =
    30


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { cells : Array Bool
    , running : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cells =
            Array.fromList <|
                List.repeat (gameWidth * gameHeight) False
      , running = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 500 Tick


type Msg
    = ClickedCell Int
    | StopSimulation
    | StartSimulation
    | Tick Time.Posix
    | Randomize
    | RandomValues (List Float)
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCell index ->
            ( -- if not model.running then
              if Maybe.withDefault False <| Array.get index model.cells then
                { model | cells = Array.set index False model.cells }

              else
                { model | cells = Array.set index True model.cells }
              --   else
              --     model
            , Cmd.none
            )

        StartSimulation ->
            ( { model | running = True }, Cmd.none )

        StopSimulation ->
            ( { model | running = False }, Cmd.none )

        Tick _ ->
            ( if model.running then
                { model | cells = updateCellsOnTick model.cells }

              else
                model
            , Cmd.none
            )

        Randomize ->
            ( model, Random.generate RandomValues <| Random.list (gameWidth * gameHeight) <| Random.float 0 1 )

        RandomValues randomFloats ->
            ( { model | cells = getRandomCells randomFloats }, Cmd.none )

        Clear ->
            init ()


getRandomCells : List Float -> Array Bool
getRandomCells randomFloats =
    Array.fromList <|
        List.map
            (\float ->
                if float > 0.8 then
                    True

                else
                    False
            )
            randomFloats


updateCellsOnTick : Array Bool -> Array Bool
updateCellsOnTick cells =
    Array.fromList <|
        List.indexedMap
            (\index value ->
                let
                    neighbours =
                        countLiveNeighbours cells <| getXAndYFromIndex index
                in
                if value && (neighbours < 2 || neighbours > 3) then
                    False

                else if value && (neighbours == 3 || neighbours == 2) then
                    True

                else if not value && neighbours == 3 then
                    True

                else
                    value
            )
        <|
            Array.toList cells


type alias Position =
    { x : Int
    , y : Int
    , index : Int
    }


getXAndYFromIndex : Int -> Position
getXAndYFromIndex index =
    let
        y =
            index // gameWidth
    in
    { x = index - (y * gameWidth), y = y, index = index }


countLiveNeighbours : Array Bool -> Position -> number
countLiveNeighbours cells pos =
    let
        counts =
            for -1
                2
                (\dy ->
                    let
                        y =
                            pos.y + dy
                    in
                    if y < gameWidth && y >= 0 then
                        for -1
                            2
                            (\dx ->
                                let
                                    x =
                                        pos.x + dx
                                in
                                if x < gameWidth && x >= 0 && (dx /= 0 || dy /= 0) then
                                    let
                                        value =
                                            Array.get (flatIndex x y) cells
                                    in
                                    case value of
                                        Just b ->
                                            if b then
                                                1

                                            else
                                                0

                                        Nothing ->
                                            0

                                else
                                    0
                            )

                    else
                        [ 0 ]
                )
    in
    List.foldl (\list -> \acc -> List.foldl (+) acc list) 0 counts


view : Model -> Html Msg
view model =
    div
        [ class "mainContainer" ]
        [ table []
            (for 0
                gameHeight
                (\y ->
                    tr []
                        (for 0
                            gameWidth
                            (\x ->
                                td
                                    [ style "background-color" <|
                                        determineBgColor model.cells x y
                                    , onClick <|
                                        ClickedCell <|
                                            flatIndex x y
                                    ]
                                    []
                            )
                        )
                )
            )
        , div
            [ class "buttons" ]
            [ button [ onClick StartSimulation ] [ text "Start" ]
            , button [ onClick StopSimulation ] [ text "Stop" ]
            , button [ onClick Randomize ] [ text "Randomize" ]
            , button [ onClick Clear ] [ text "Clear" ]
            , determineStatus model.running
            ]
        ]


determineStatus : Bool -> Html msg
determineStatus running =
    if running then
        span [] [ text "Running..." ]

    else
        span [] [ text "Waiting..." ]


determineBgColor : Array Bool -> Int -> Int -> String
determineBgColor cells x y =
    if Maybe.withDefault True <| Array.get (flatIndex x y) cells then
        "#080"

    else
        "#ddd"


flatIndex : number -> number -> number
flatIndex x y =
    y * gameWidth + x


for : number -> number -> (number -> a) -> List a
for curr end func =
    if curr == end - 1 then
        [ func curr ]

    else
        func curr :: for (curr + 1) end func
