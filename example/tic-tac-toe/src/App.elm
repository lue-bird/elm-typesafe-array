module App exposing (Field(..), GameOver(..), Player(..), isGameOver, main)

import ArraySized exposing (ArraySized)
import Browser
import Element as Ui
import Element.Background as Background
import Element.Font as Font
import Element.Input as UiInput
import Linear exposing (Direction(..))
import Maybe.Extra as Maybe
import N exposing (Exactly, In, N, N0, N2, N3, On, n2, n3)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Stack
import Toop


type alias Model =
    RecordWithoutConstructorFunction
        { board : Board
        , gameStage : GameStage
        }


initialModel : Model
initialModel =
    { gameStage = Playing O
    , board =
        ArraySized.repeat
            (ArraySized.repeat FieldNotSet n3)
            n3
    }


type alias Board =
    ArraySized (ArraySized Field (Exactly (On N3))) (Exactly (On N3))


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


main : Program () Model Event
main =
    Browser.document
        { init = \() -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Event
    = FieldSetByPlayer
        ( N (In (On N0) (On N2))
        , N (In (On N0) (On N2))
        )
        Player
    | ClearBoardClicked


update : Event -> Model -> ( Model, Cmd Event )
update msg =
    case msg of
        FieldSetByPlayer ( x, y ) player ->
            \model ->
                let
                    field =
                        model.board
                            |> ArraySized.element ( Up, x )
                            |> ArraySized.element ( Up, y )
                in
                ( case field of
                    FieldSet _ ->
                        model

                    FieldNotSet ->
                        let
                            updatedBoard =
                                model.board
                                    |> ArraySized.elementAlter ( Up, x )
                                        (ArraySized.elementReplace ( Up, y )
                                            (\() -> FieldSet player)
                                        )
                        in
                        { board = updatedBoard
                        , gameStage =
                            case isGameOver updatedBoard of
                                Nothing ->
                                    Playing (playerOpponent player)

                                Just gameOver ->
                                    GameOver gameOver
                        }
                , Cmd.none
                )

        ClearBoardClicked ->
            \model -> ( initialModel, Cmd.none )


isGameOver : Board -> Maybe GameOver
isGameOver board =
    let
        won fields =
            case fields |> ArraySized.to3 of
                Toop.T3 (FieldSet O) (FieldSet O) (FieldSet O) ->
                    Just O

                Toop.T3 (FieldSet X) (FieldSet X) (FieldSet X) ->
                    Just X

                _ ->
                    Nothing

        rowWon =
            ArraySized.upTo n2
                |> ArraySized.map
                    (\i ->
                        board
                            |> ArraySized.element ( Up, i )
                            |> won
                    )

        columnWon =
            ArraySized.upTo n2
                |> ArraySized.map
                    (\i ->
                        board
                            |> ArraySized.map (ArraySized.element ( Up, i ))
                            |> won
                    )

        diagonal xDirection =
            ArraySized.upTo n2
                |> ArraySized.map
                    (\i ->
                        board
                            |> ArraySized.element ( xDirection, i )
                            |> ArraySized.element ( Up, i )
                    )

        noEmptyFields =
            ArraySized.map ArraySized.toList
                >> ArraySized.toList
                >> List.concat
                >> List.all ((/=) FieldNotSet)

        wonPlayer =
            Maybe.orList
                ([ [ diagonal Up |> won
                   , diagonal Down |> won
                   ]
                 , rowWon |> ArraySized.toList
                 , columnWon |> ArraySized.toList
                 ]
                    |> List.concat
                )
    in
    case wonPlayer of
        Just playerWon ->
            playerWon |> PlayerWon |> Just

        Nothing ->
            if noEmptyFields board then
                Draw |> Just

            else
                Nothing


playerOpponent : Player -> Player
playerOpponent =
    \player ->
        case player of
            O ->
                X

            X ->
                O


playerToString : Player -> String
playerToString player =
    case player of
        O ->
            -- 🞅 | ⭘ | ○ | 𝡶
            "⭘"

        X ->
            -- ⨯
            "⯅"


view : Model -> Browser.Document Event
view { board, gameStage } =
    { title = "tic tac toe"
    , body =
        [ (case gameStage of
            Playing playing ->
                playerToString playing ++ "'s turn!"

            GameOver gameOver ->
                [ case gameOver of
                    PlayerWon winner ->
                        playerToString winner ++ " won!"

                    Draw ->
                        "Draw!"
                , " Click any field to reset."
                ]
                    |> String.concat
          )
            |> Ui.text
            |> Ui.el
                [ Ui.centerX
                , Font.size 40
                , Font.color (Ui.rgb 1 1 1)
                ]
        , board |> viewBoard { gameStage = gameStage }
        ]
            |> Ui.column
                [ Ui.centerX
                , Ui.centerY
                , Ui.spacing 30
                ]
            |> Ui.layoutWith
                { options =
                    [ Ui.focusStyle
                        { borderColor = Nothing
                        , backgroundColor = Ui.rgb 0 0.6 0.3 |> Just
                        , shadow = Nothing
                        }
                    ]
                }
                [ Background.color (Ui.rgb 0 0 0)
                ]
            |> List.singleton
    }


fieldSpacing : number_
fieldSpacing =
    6


fieldSize : number_
fieldSize =
    100


boardSize : number_
boardSize =
    fieldSize * 3 + fieldSpacing * 2


fieldToShape : Field -> Ui.Element event_
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
                    , Font.size ((fieldSize * 0.7) |> round)
                    ]


viewBoard : { gameStage : GameStage } -> Board -> Ui.Element Event
viewBoard { gameStage } =
    \board ->
        let
            viewField ( x, y ) =
                let
                    action =
                        case gameStage of
                            Playing player ->
                                player |> FieldSetByPlayer ( x, y )

                            GameOver _ ->
                                ClearBoardClicked
                in
                { onPress = action |> Just
                , label =
                    board
                        |> ArraySized.element ( Up, x )
                        |> ArraySized.element ( Up, y )
                        |> fieldToShape
                }
                    |> UiInput.button
                        [ Ui.width Ui.fill
                        , Ui.height Ui.fill
                        , Background.color (Ui.rgb 0 0 0)
                        ]
        in
        ArraySized.upTo n2
            |> ArraySized.map
                (\x ->
                    ArraySized.upTo n2
                        |> ArraySized.map (\y -> viewField ( x, y ))
                        |> ArraySized.toList
                        |> Ui.column
                            [ Ui.width Ui.fill
                            , Ui.height Ui.fill
                            , Ui.spacing fieldSpacing
                            ]
                )
            |> ArraySized.toList
            |> Ui.row
                [ Ui.width (Ui.px boardSize)
                , Ui.height (Ui.px boardSize)
                , Ui.centerX
                , Ui.centerY
                , Background.color (Ui.rgb 1 1 1)
                , Ui.spacing fieldSpacing
                , Font.color (Ui.rgb 1 1 1)
                ]
