module GenerateForElmBoundedArray exposing (main)

{-| Helps you generate the source code of `Arr.from1` to `from16` & the module `ArgN.Arguments`.

Thanks to [`the-sett/elm-syntax-dsl`](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest/)!

-}

import Browser
import Element as Ui
import Element.Background as UiBg
import Element.Font as UiFont
import Element.Input as UiInput
import Elm.CodeGen as Gen exposing (applyBinOp, construct, fqConstruct, fqNamedPattern, fqTyped, fun, lambda, letDestructuring, letExpr, markdown, namedPattern, parens, parensPattern, piper, typeVar, typed, val, varPattern)
import Extra.GenerateElm exposing (..)
import Extra.Ui as Ui
import File.Download
import Html exposing (Html)
import String exposing (concat)
import Task
import Time
import Zip


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { fromAndToXShownOrFolded :
        ShownOrFolded (Ui.Element Msg)
    , argumentsModuleShownOrFolded :
        ShownOrFolded (Ui.Element Msg)
    }


type ShownOrFolded content
    = Shown content
    | Folded



--


init : ( Model, Cmd Msg )
init =
    ( { fromAndToXShownOrFolded = Folded
      , argumentsModuleShownOrFolded = Folded
      }
    , Cmd.none
    )


type Msg
    = DownloadModules
    | DownloadModulesAtTime ( Time.Zone, Time.Posix )
    | SwitchVisibleModule ModulesInElmArrs


type ModulesInElmArrs
    = FromAndToX
    | Arguments


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DownloadModules ->
            ( model
            , Task.perform
                (\time -> DownloadModulesAtTime time)
                (Time.here
                    |> Task.andThen
                        (\here ->
                            Time.now
                                |> Task.map (\now -> ( here, now ))
                        )
                )
            )

        DownloadModulesAtTime time ->
            ( model
            , File.Download.bytes
                "elm-arrs-modules.zip"
                "application/zip"
                (let
                    toZipEntry moduleFile =
                        zipEntryFromModule time moduleFile
                 in
                 Zip.fromEntries
                    [ toZipEntry fromAndToX
                    , toZipEntry argumentsModule
                    ]
                    |> Zip.toBytes
                )
            )

        SwitchVisibleModule moduleKind ->
            ( case moduleKind of
                FromAndToX ->
                    { model
                        | fromAndToXShownOrFolded =
                            switchShownOrFolded
                                (.fromAndToXShownOrFolded model)
                                viewFromAndToX
                    }

                Arguments ->
                    { model
                        | argumentsModuleShownOrFolded =
                            switchShownOrFolded
                                (.argumentsModuleShownOrFolded model)
                                viewArgumentsModule
                    }
            , Cmd.none
            )


switchShownOrFolded :
    ShownOrFolded content
    -> content
    -> ShownOrFolded content
switchShownOrFolded visibility content =
    case visibility of
        Shown _ ->
            Folded

        Folded ->
            Shown content



--


natNAnn : Gen.TypeAnnotation -> Gen.TypeAnnotation
natNAnn n =
    typed "Nat" [ n ]


nAnn : Int -> Gen.TypeAnnotation
nAnn n =
    typed "N"
        [ natXAnn n
        , natXPlusAnn n (typeVar "more")
        , isAnn n "a"
        , isAnn n "b"
        ]


isAnn : Int -> String -> Gen.TypeAnnotation
isAnn n var =
    typed "Is"
        [ typeVar var
        , toAnn
        , natXPlusAnn n (typeVar var)
        ]


toAnn : Gen.TypeAnnotation
toAnn =
    typed "To" []


natXAnn : Int -> Gen.TypeAnnotation
natXAnn x =
    typed ([ "Nat", x |> String.fromInt ] |> concat)
        []


natXPlusAnn : Int -> Gen.TypeAnnotation -> Gen.TypeAnnotation
natXPlusAnn x more =
    case x of
        0 ->
            more

        _ ->
            typed
                ([ "Nat", x |> String.fromInt, "Plus" ]
                    |> concat
                )
                [ more ]


arrAnn : Gen.TypeAnnotation -> Gen.TypeAnnotation -> Gen.TypeAnnotation
arrAnn length element =
    typed "Arr" [ length, element ]


onlyAnn : Gen.TypeAnnotation -> Gen.TypeAnnotation
onlyAnn nat =
    typed "Only" [ nat ]



--


viewArgumentsModule : Ui.Element msg_
viewArgumentsModule =
    Ui.module_ argumentsModule


argumentsModule : Module Never
argumentsModule =
    { name = [ "Arguments" ]
    , roleInPackage = PackageInternalModule
    , imports = []
    , declarations =
        let
            applyPreset x applyXMinus1 =
                packageInternalExposedFunDecl
                    (let
                        lastTypeVar =
                            charIndex (x + 1)
                                |> String.fromChar
                                |> typeVar

                        secondLastTypeVar =
                            charIndex x
                                |> String.fromChar
                                |> typeVar
                     in
                     funAnn
                        ([ funAnn
                            (charPrefixed (\i -> typeVar (i "")) (x - 1))
                            secondLastTypeVar
                         , funAnn [ secondLastTypeVar ] lastTypeVar
                         ]
                            ++ charPrefixed (\i -> typeVar (i "")) (x - 1)
                        )
                        lastTypeVar
                    )
                    ("apply" ++ String.fromInt x)
                    [ varPattern "f", varPattern "more" ]
                    (lambda [ varPattern "a" ]
                        applyXMinus1
                    )
        in
        applyPreset 1
            (applyBinOp
                (parens (construct "f" [ val "a" ]))
                piper
                (val "more")
            )
            :: (List.range 2 16
                    |> List.map
                        (\x ->
                            applyPreset x
                                (construct ("apply" ++ String.fromInt (x - 1))
                                    [ parens (construct "f" [ val "a" ])
                                    , val "more"
                                    ]
                                )
                        )
               )
    }


viewFromAndToX : Ui.Element msg_
viewFromAndToX =
    Ui.module_ fromAndToX


type FromAndToXTag
    = FromXTag
    | ToXTag


fromAndToX : Module FromAndToXTag
fromAndToX =
    { name = [ "FromAndToX" ]
    , roleInPackage =
        PackageExposedModule
            { moduleComment = \_ -> []
            }
    , imports =
        []
    , declarations =
        let
            fromXPreset x implementation =
                packageExposedFunDecl FromXTag
                    [ markdown
                        (case x of
                            1 ->
                                "Create an `Arr` with exactly 1 element."

                            _ ->
                                "Create an `Arr` with exactly "
                                    ++ String.fromInt x
                                    ++ " elements in this order."
                        )
                    ]
                    (funAnn
                        (List.repeat x (typeVar "element"))
                        (typed "Arr"
                            [ typed "In"
                                [ natXAnn x
                                , natXPlusAnn x (typeVar "a_")
                                ]
                            , typeVar "element"
                            ]
                        )
                    )
                    ([ "from", x |> String.fromInt ] |> concat)
                    []
                    implementation
        in
        [ fromXPreset 1
            (lambda [ varPattern "a" ]
                (applyBinOp
                    (val "empty")
                    piper
                    (construct "push" [ val "a" ])
                )
            )
            :: (List.range 2 16
                    |> List.map
                        (\x ->
                            fromXPreset x
                                (construct ([ "apply", (x - 1) |> String.fromInt ] |> concat)
                                    [ fun ([ "from", (x - 1) |> String.fromInt ] |> concat)
                                    , lambda [ varPattern "init" ]
                                        (lambda [ varPattern "last" ]
                                            (applyBinOp (val "init")
                                                piper
                                                (construct "push" [ val "last" ])
                                            )
                                        )
                                    ]
                                )
                        )
               )
        , List.range 2 14
            |> List.map
                (\i ->
                    let
                        t n =
                            [ "T", n |> String.fromInt ]
                                |> concat
                    in
                    packageExposedFunDecl ToXTag
                        [ markdown
                            ([ "Transform the `Arr` into a `Toop."
                             , t i
                             , "`. This makes accessing elements and pattern matching easier."
                             ]
                                |> concat
                            )
                        ]
                        (funAnn
                            [ arrAnn (onlyAnn (natXAnn i))
                                (typeVar "element")
                            ]
                            (fqTyped [ "Toop" ]
                                (t i)
                                (List.repeat i (typeVar "element"))
                            )
                        )
                        ([ "to", i |> String.fromInt ] |> concat)
                        []
                        (lambda [ varPattern "arr" ]
                            (fqConstruct [ "Toop" ]
                                (t i)
                                (List.range 0 (i - 1)
                                    |> List.map
                                        (\argI ->
                                            construct "at"
                                                [ val ([ "nat", argI |> String.fromInt ] |> concat)
                                                , val "FirstToLast"
                                                , val "arr"
                                                ]
                                                |> parens
                                        )
                                )
                            )
                        )
                )
        ]
            |> List.concat
    }



--


charIndex : Int -> Char
charIndex i =
    i + Char.toCode 'a' |> Char.fromCode


charPrefixed : ((String -> String) -> a) -> Int -> List a
charPrefixed use last =
    List.range 0 last
        |> List.map
            (charIndex >> (\i -> use (String.cons i)))


view : Model -> Html Msg
view { fromAndToXShownOrFolded, argumentsModuleShownOrFolded } =
    Ui.layoutWith
        { options =
            [ Ui.focusStyle
                { borderColor = Just (Ui.rgba 0 1 1 0.38)
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        []
        (Ui.column
            [ Ui.paddingXY 40 60
            , Ui.spacing 32
            , Ui.width Ui.fill
            , Ui.height Ui.fill
            , UiBg.color (Ui.rgb255 35 36 31)
            , UiFont.color (Ui.rgb 1 1 1)
            ]
            [ Ui.el
                [ UiFont.size 40
                , UiFont.family [ UiFont.monospace ]
                ]
                (Ui.text "elm-bounded-nat modules")
            , UiInput.button
                [ Ui.padding 16
                , UiBg.color (Ui.rgba 1 0.4 0 0.6)
                ]
                { onPress = Just DownloadModules
                , label = Ui.text "⬇ download elm modules"
                }
            , Ui.column
                [ Ui.width Ui.fill
                ]
                (Ui.el [ Ui.paddingXY 0 6 ]
                    (Ui.text "📂 preview modules")
                    :: (let
                            switchButton text switch =
                                Ui.el
                                    [ Ui.width Ui.fill
                                    , Ui.paddingXY 0 6
                                    , Ui.moveUp 6
                                    ]
                                    (UiInput.button
                                        [ UiBg.color (Ui.rgba 1 0.4 0 0.6)
                                        , Ui.padding 12
                                        , Ui.width Ui.fill
                                        ]
                                        { onPress = Just switch
                                        , label =
                                            Ui.el
                                                [ UiFont.family [ UiFont.monospace ] ]
                                                (Ui.text text)
                                        }
                                    )

                            viewAccordingToShownOrFolded visibility name switch =
                                case visibility of
                                    Shown ui ->
                                        Ui.row
                                            [ Ui.height Ui.fill, Ui.width Ui.fill ]
                                            [ Ui.el
                                                [ Ui.width (Ui.px 1)
                                                , UiBg.color (Ui.rgba 1 0.4 0 0.6)
                                                , Ui.height Ui.fill
                                                ]
                                                Ui.none
                                            , Ui.column [ Ui.width Ui.fill ]
                                                [ switchButton ("⌃ " ++ name) switch
                                                , Ui.el [ Ui.moveRight 5 ] ui
                                                ]
                                            ]

                                    Folded ->
                                        switchButton ("⌄ " ++ name) switch
                        in
                        [ ( fromAndToXShownOrFolded
                          , ( "FromAndToX", FromAndToX )
                          )
                        , ( argumentsModuleShownOrFolded
                          , ( "Arguments", Arguments )
                          )
                        ]
                            |> List.map
                                (\( visibility, ( name, moduleKind ) ) ->
                                    viewAccordingToShownOrFolded visibility
                                        name
                                        (SwitchVisibleModule moduleKind)
                                )
                       )
                )
            ]
        )
