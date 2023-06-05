module ArraySized.Generate exposing (main)

{-| Helps you generate the source code of `ArraySized.l<x>`, `.to<x>` 2 to 16

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
    { x : Int
    , fromAndToXShownOrFolded : ShownOrFolded
    , lShownOrFolded : ShownOrFolded
    }


type ShownOrFolded
    = Shown
    | Folded



--


init : ( Model, Cmd Msg )
init =
    ( { x = 310
      , fromAndToXShownOrFolded = Folded
      , lShownOrFolded = Folded
      }
    , Cmd.none
    )


type Msg
    = XEdited String
    | ModulesDownloadRequested
    | ModulesDownloadRequestedAtTime ( Time.Zone, Time.Posix )
    | ModuleFoldingSwitchRequested ModulesInElmArrs


type ModulesInElmArrs
    = LAndTo16
    | L


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        XEdited xTextNew ->
            ( case xTextNew |> String.toInt of
                Nothing ->
                    model

                Just xNew ->
                    { model | x = xNew }
            , Cmd.none
            )

        ModulesDownloadRequested ->
            ( model
            , Task.perform
                (\time -> ModulesDownloadRequestedAtTime time)
                (Time.here
                    |> Task.andThen
                        (\here ->
                            Time.now
                                |> Task.map (\now -> ( here, now ))
                        )
                )
            )

        ModulesDownloadRequestedAtTime time ->
            ( model
            , File.Download.bytes
                "typesafe-array-generated.zip"
                "application/zip"
                (let
                    toZipEntry moduleFile =
                        zipEntryFromModule time moduleFile
                 in
                 Zip.fromEntries
                    [ toZipEntry lAndTo16
                    , toZipEntry (l model.x)
                    ]
                    |> Zip.toBytes
                )
            )

        ModuleFoldingSwitchRequested moduleKind ->
            ( case moduleKind of
                LAndTo16 ->
                    { model
                        | fromAndToXShownOrFolded =
                            model.fromAndToXShownOrFolded |> shownOrFoldedSwitch
                    }

                L ->
                    { model
                        | lShownOrFolded =
                            model.lShownOrFolded |> shownOrFoldedSwitch
                    }
            , Cmd.none
            )


shownOrFoldedSwitch : ShownOrFolded -> ShownOrFolded
shownOrFoldedSwitch =
    \folding ->
        case folding of
            Shown ->
                Folded

            Folded ->
                Shown



--


nType : Gen.TypeAnnotation -> Gen.TypeAnnotation
nType n =
    typed "N" [ n ]


inType : Gen.TypeAnnotation -> Gen.TypeAnnotation -> Gen.TypeAnnotation
inType min max =
    typed "In" [ min, max ]


onType : Gen.TypeAnnotation -> Gen.TypeAnnotation
onType number =
    typed "On" [ number ]


upType : Gen.TypeAnnotation -> Gen.TypeAnnotation -> Gen.TypeAnnotation
upType from to =
    typed "Up" [ from, toType, to ]


toType : Gen.TypeAnnotation
toType =
    typed "To" []


upNType : Int -> String -> Gen.TypeAnnotation
upNType n var =
    typed ([ "Up", n |> String.fromInt ] |> String.concat)
        [ typeVar var ]


nXType : Int -> Gen.TypeAnnotation
nXType x =
    typed ([ "N", x |> String.fromInt ] |> concat)
        []


addXType : Int -> Gen.TypeAnnotation -> Gen.TypeAnnotation
addXType x more =
    case x of
        0 ->
            more

        _ ->
            typed
                ([ "Add", x |> String.fromInt ] |> concat)
                [ more ]


arraySizedType : Gen.TypeAnnotation -> Gen.TypeAnnotation -> Gen.TypeAnnotation
arraySizedType element length =
    typed "ArraySized" [ element, length ]


inUpType : Int -> Gen.TypeAnnotation
inUpType n =
    inType (upNType n "minX_") (upNType n "maxX_")



--


type FromAndToXTag
    = FromXTag
    | ToXTag


lAndTo16 : Module FromAndToXTag
lAndTo16 =
    { name = [ "ArraySized" ]
    , roleInPackage =
        PackageExposedModule
            { moduleComment =
                \_ ->
                    [ markdown "`l<n>`, `to<n>` declarations exposed from inside `module ArraySized`"
                    ]
            }
    , imports = []
    , declarations =
        let
            lX x implementation =
                packageExposedFunDecl FromXTag
                    (x |> fromNDoc)
                    (funAnn
                        (List.repeat x (typeVar "element"))
                        (arraySizedType (typeVar "element") (inUpType x))
                    )
                    ([ "l", x |> String.fromInt ] |> concat)
                    (List.range 0 (x - 1)
                        |> List.map
                            (\i -> [ "a", i |> String.fromInt ] |> String.concat)
                    )
                    implementation
        in
        [ List.range 2 16
            |> List.map
                (\x ->
                    lX x
                        (applyBinOp
                            (case x of
                                1 ->
                                    Gen.val "empty"

                                x2AtLeast ->
                                    construct
                                        ([ "l", (x2AtLeast - 1) |> String.fromInt ] |> concat)
                                        (List.range 0 (x - 2)
                                            |> List.map
                                                (\i -> val ("a" ++ (i |> String.fromInt)))
                                        )
                            )
                            piper
                            (construct "push" [ val ("a" ++ (x - 1 |> String.fromInt)) ])
                        )
                )
        , List.range 2 16
            |> List.map
                (\i ->
                    let
                        t n =
                            [ "T", n |> String.fromInt ]
                                |> concat
                    in
                    packageExposedFunDecl ToXTag
                        [ markdown
                            ([ "Transform into a `Toop."
                             , t i
                             , "` to simplify accessing elements, pattern matching"
                             ]
                                |> concat
                            )
                        ]
                        (funAnn
                            [ arraySizedType
                                (typeVar "element")
                                -- (In (On (Add1 minFrom1_)) (Up maxTo1_ To N1))
                                (inType
                                    (Gen.typeVar
                                        ([ "minFrom", i |> String.fromInt, "_" ] |> String.concat)
                                        |> addXType i
                                        |> onType
                                    )
                                    (upType
                                        (Gen.typeVar
                                            ([ "maxTo", i |> String.fromInt, "_" ] |> String.concat)
                                        )
                                        (nXType i)
                                    )
                                )
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
                                (List.range 1 i
                                    |> List.map
                                        (\argI ->
                                            applyBinOp
                                                (val "arr")
                                                piper
                                                (construct "element"
                                                    [ Gen.tuple
                                                        [ val "Up"
                                                        , val ([ "n", argI |> String.fromInt ] |> concat)
                                                        ]
                                                    ]
                                                )
                                                |> parens
                                        )
                                )
                            )
                        )
                )
        ]
            |> List.concat
    }


fromNDoc : Int -> Comment Gen.DocComment
fromNDoc n =
    [ markdown
        (case n of
            1 ->
                "Create with only 1 single given element"

            nAtLeast2 ->
                [ "Create with "
                , nAtLeast2 |> String.fromInt
                , " given elements in the order they are supplied"
                ]
                    |> String.concat
        )
    ]


l : Int -> Module FromAndToXTag
l x =
    { name = [ "ArraySized", "Local" ]
    , roleInPackage = PackageInternalModule
    , imports =
        [ Gen.importStmt [ "ArraySized" ]
            Nothing
            ([ List.map Gen.funExpose [ "empty", "attach" ]
             , List.map Gen.typeOrAliasExpose [ "ArraySized", "In" ]
             ]
                |> List.concat
                |> Gen.exposeExplicit
                |> Just
            )
        , Gen.importStmt [ "Ns" ]
            Nothing
            (List.map Gen.typeOrAliasExpose
                [ "N" ++ (x |> String.fromInt)
                , "Add" ++ (x |> String.fromInt)
                ]
                |> Gen.exposeExplicit
                |> Just
            )
        ]
    , declarations =
        let
            chunksOf16 =
                x // 16

            remainder =
                x |> remainderBy 16
        in
        [ packageExposedFunDecl FromXTag
            (x |> fromNDoc)
            (funAnn
                (List.repeat x (typeVar "element"))
                (arraySizedType (typeVar "element") (inUpType x))
            )
            ([ "l", x |> String.fromInt ] |> concat)
            (List.range 0 (x - 1)
                |> List.map
                    (\i -> [ "a", i |> String.fromInt ] |> String.concat)
            )
            (Gen.binOpChain
                (case remainder of
                    0 ->
                        Gen.val "empty"

                    remainder1AtLeast ->
                        construct
                            ([ "l", (remainder1AtLeast - 1) |> String.fromInt ] |> concat)
                            (List.range 0 (remainder - 1)
                                |> List.map
                                    (\i -> val ("a" ++ (i |> String.fromInt)))
                            )
                )
                piper
                (List.range 0 (chunksOf16 - 1)
                    |> List.map (\chunk -> remainder + chunk * 16)
                    |> List.map
                        (\chunkStart ->
                            construct "attach"
                                [ val "Up"
                                , construct "l16"
                                    (List.range chunkStart (chunkStart + 15)
                                        |> List.map (\i -> val ("a" ++ (i |> String.fromInt)))
                                    )
                                    |> Gen.parens
                                ]
                        )
                )
            )
        ]
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
view { fromAndToXShownOrFolded, lShownOrFolded, x } =
    [ "typesafe-array"
        |> Ui.text
        |> Ui.el
            [ UiFont.size 40
            , UiFont.family [ UiFont.monospace ]
            ]
    , UiInput.button
        [ Ui.padding 16
        , UiBg.color (Ui.rgba 1 0.4 0 0.6)
        ]
        { onPress = Just ModulesDownloadRequested
        , label = Ui.text "⬇ download"
        }
    , UiInput.text
        [ Ui.padding 16
        , UiBg.color (Ui.rgba 1 0.4 0 0.6)
        ]
        { onChange = XEdited
        , text = x |> String.fromInt
        , label = Ui.text "number to generate for" |> UiInput.labelAbove []
        , placeholder = Nothing
        }
    , ([ [ "📂 preview"
            |> Ui.text
            |> Ui.el [ Ui.paddingXY 0 6 ]
         ]
       , let
            switchButton text switch =
                { onPress = Just switch
                , label =
                    text
                        |> Ui.text
                        |> Ui.el
                            [ UiFont.family [ UiFont.monospace ] ]
                }
                    |> UiInput.button
                        [ UiBg.color (Ui.rgba 1 0.4 0 0.6)
                        , Ui.padding 12
                        , Ui.width Ui.fill
                        ]
                    |> Ui.el
                        [ Ui.width Ui.fill
                        , Ui.paddingXY 0 6
                        , Ui.moveUp 6
                        ]

            viewAccordingToShownOrFolded { kind, name, folding, module_ } =
                case folding of
                    Shown ->
                        [ Ui.el
                            [ Ui.width (Ui.px 1)
                            , UiBg.color (Ui.rgba 1 0.4 0 0.6)
                            , Ui.height Ui.fill
                            ]
                            Ui.none
                        , [ kind |> ModuleFoldingSwitchRequested |> switchButton ("⌃ " ++ name)
                          , Ui.el [ Ui.moveRight 5 ] (module_ |> Ui.module_)
                          ]
                            |> Ui.column [ Ui.width Ui.fill ]
                        ]
                            |> Ui.row
                                [ Ui.height Ui.fill, Ui.width Ui.fill ]

                    Folded ->
                        kind |> ModuleFoldingSwitchRequested |> switchButton ("⌄ " ++ name)
         in
         [ { kind = L
           , folding = lShownOrFolded
           , name = [ "l", x |> String.fromInt ] |> String.concat
           , module_ = l x
           }
         , { kind = LAndTo16
           , folding = fromAndToXShownOrFolded
           , name = "l2 → l16, to2 → to16"
           , module_ = lAndTo16
           }
         ]
            |> List.map viewAccordingToShownOrFolded
       ]
        |> List.concat
      )
        |> Ui.column
            [ Ui.width Ui.fill ]
    ]
        |> Ui.column
            [ Ui.paddingXY 40 60
            , Ui.spacing 32
            , Ui.width Ui.fill
            , Ui.height Ui.fill
            , UiBg.color (Ui.rgb255 35 36 31)
            , UiFont.color (Ui.rgb 1 1 1)
            ]
        |> Ui.layoutWith
            { options =
                [ Ui.focusStyle
                    { borderColor = Just (Ui.rgba 0 1 1 0.38)
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            []
