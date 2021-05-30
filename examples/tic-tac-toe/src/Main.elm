module Main exposing (Field(..), GameOver(..), Player(..), isGameOver, main)

import Arr exposing (Arr)
import Array
import Browser
import Element as Ui
import Element.Background as Background
import Element.Font as Font
import Element.Input as UiInput
import LinearDirection exposing (LinearDirection(..))
import Maybe.Extra as Maybe
import NNats exposing (..)
import Nat exposing (In, Nat, Only)
import TypeNats exposing (..)


type alias Model =
    { board : Board
    , gameStage : GameStage
    }


initialModel : Model
initialModel =
    { gameStage = Playing O
    , board =
        Arr.repeat nat3
            (Arr.repeat nat3 FieldNotSet)
    }


type alias Board =
    Arr (Only Nat3) (Arr (Only Nat3) Field)


type Field
    = FieldNotSet
    | FieldSet Player


type Player
    = X
    | O


type GameStage
    = GameOver GameOver
    | Playing Player


type GameOver
    = PlayerWon Player
    | Draw


main : Program () Model Msg
main =
    Browser.document
        { init = \() -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = PlayerSetsField (Nat (In Nat0 Nat2)) (Nat (In Nat0 Nat2)) Player
    | ClearBoard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerSetsField x y player ->
            ( let
                field =
                    model.board
                        |> Arr.at x FirstToLast
                        |> Arr.at y FirstToLast
              in
              case field of
                FieldNotSet ->
                    let
                        updatedBoard =
                            model.board
                                |> Arr.updateAt x
                                    FirstToLast
                                    (Arr.replaceAt y
                                        FirstToLast
                                        (FieldSet player)
                                    )
                    in
                    { board = updatedBoard
                    , gameStage =
                        case isGameOver updatedBoard of
                            Nothing ->
                                Playing (otherPlayer player)

                            Just gameOver ->
                                GameOver gameOver
                    }

                FieldSet _ ->
                    model
            , Cmd.none
            )

        ClearBoard ->
            ( initialModel
            , Cmd.none
            )


isGameOver : Board -> Maybe GameOver
isGameOver board =
    let
        allXorO fields =
            if fields == Arr.repeat nat3 (FieldSet O) then
                Just O

            else if fields == Arr.repeat nat3 (FieldSet X) then
                Just X

            else
                Nothing

        wonInRow =
            Nat.range nat0 nat2
                |> List.map
                    (\i ->
                        board
                            |> Arr.at i FirstToLast
                            |> allXorO
                    )

        wonInColumn =
            Nat.range nat0 nat2
                |> List.map
                    (\i ->
                        board
                            |> Arr.map (Arr.at i FirstToLast)
                            |> allXorO
                    )

        wonDiagonally xDirection =
            Arr.nats nat3
                |> Arr.map
                    (\i ->
                        board
                            |> Arr.at i xDirection
                            |> Arr.at i FirstToLast
                    )
                |> allXorO

        noEmptyFields =
            Arr.map (Arr.toArray >> Array.toList)
                >> Arr.toArray
                >> Array.toList
                >> List.concat
                >> List.all ((/=) FieldNotSet)
    in
    Maybe.orList
        (wonDiagonally FirstToLast
            :: wonDiagonally LastToFirst
            :: wonInRow
            ++ wonInColumn
        )
        |> Maybe.map PlayerWon
        |> Maybe.orElse
            (if noEmptyFields board then
                Just Draw

             else
                Nothing
            )


otherPlayer : Player -> Player
otherPlayer player =
    case player of
        O ->
            X

        X ->
            O


playerToString : Player -> String
playerToString player =
    case player of
        O ->
            "🞅"

        X ->
            "⨯"


view : Model -> Browser.Document Msg
view { board, gameStage } =
    { title = "tic tac toe"
    , body =
        Ui.column
            [ Ui.centerX
            , Ui.centerY
            , Ui.spacing 30
            ]
            [ (case gameStage of
                Playing playing ->
                    playerToString playing ++ "'s turn!"

                GameOver gameOver ->
                    (case gameOver of
                        PlayerWon winner ->
                            playerToString winner ++ " won!"

                        Draw ->
                            "Draw!"
                    )
                        ++ " Click any field to reset."
              )
                |> Ui.text
                |> Ui.el
                    [ Ui.centerX
                    , Font.size 40
                    , Font.color (Ui.rgb 1 1 1)
                    ]
            , viewBoard board { gameStage = gameStage }
            ]
            |> Ui.layoutWith
                { options =
                    [ Ui.focusStyle
                        { borderColor = Nothing
                        , backgroundColor = Just (Ui.rgb 0 0.6 0.3)
                        , shadow = Nothing
                        }
                    ]
                }
                [ Background.color (Ui.rgb 0 0 0) ]
            |> List.singleton
    }


viewBoard : Board -> { gameStage : GameStage } -> Ui.Element Msg
viewBoard board { gameStage } =
    let
        spacing =
            6

        fieldSize =
            100

        viewField x y =
            let
                fieldToShape field =
                    case field of
                        FieldNotSet ->
                            Ui.none

                        FieldSet playerOnTheField ->
                            playerToString playerOnTheField
                                |> Ui.text
                                |> Ui.el
                                    [ Ui.centerX
                                    , Ui.centerY
                                    , Ui.moveDown 5
                                    , Font.size ((fieldSize * 0.7) |> round)
                                    ]
            in
            UiInput.button
                [ Ui.width Ui.fill
                , Ui.height Ui.fill
                , Background.color (Ui.rgb 0 0 0)
                ]
                { onPress =
                    case gameStage of
                        Playing playing ->
                            PlayerSetsField x y playing |> Just

                        GameOver _ ->
                            ClearBoard |> Just
                , label =
                    board
                        |> Arr.at x FirstToLast
                        |> Arr.at y FirstToLast
                        |> fieldToShape
                }

        boardSize =
            fieldSize * 3 + spacing * 2
    in
    Nat.range nat0 nat2
        |> List.map
            (\x ->
                Nat.range nat0 nat2
                    |> List.map (\y -> viewField x y)
                    |> Ui.column
                        [ Ui.width Ui.fill
                        , Ui.height Ui.fill
                        , Ui.spacing spacing
                        ]
            )
        |> Ui.row
            [ Ui.width (Ui.px boardSize)
            , Ui.height (Ui.px boardSize)
            , Ui.centerX
            , Ui.centerY
            , Background.color (Ui.rgb 1 1 1)
            , Ui.spacing spacing
            , Font.color (Ui.rgb 1 1 1)
            ]
