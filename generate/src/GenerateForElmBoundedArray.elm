module GenerateForElmBoundedArray exposing (main)

{-| Helps you generate the source code of `Arr.from1` to `from16` & the module `ArgN.Arguments`.

Thanks to [`the-sett/elm-syntax-dsl`](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest/)!

-}

import Browser
import Bytes.Encode
import Element as Ui
import Element.Background as UiBg
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import Elm.CodeGen
    exposing
        ( access
        , and
        , append
        , applyBinOp
        , binOp
        , binOpChain
        , caseExpr
        , char
        , code
        , composel
        , composer
        , cons
        , construct
        , customTypeDecl
        , equals
        , extRecordAnn
        , fqConstruct
        , fqFun
        , fqNamedPattern
        , fqTyped
        , fun
        , funExpose
        , importStmt
        , int
        , intAnn
        , lambda
        , letDestructuring
        , letExpr
        , letFunction
        , letVal
        , list
        , listAnn
        , listPattern
        , markdown
        , maybeAnn
        , minus
        , namedPattern
        , normalModule
        , openTypeExpose
        , parens
        , pipel
        , piper
        , plus
        , record
        , recordAnn
        , recordPattern
        , tuple
        , tupleAnn
        , tuplePattern
        , typeOrAliasExpose
        , typeVar
        , typed
        , unConsPattern
        , unit
        , unitAnn
        , val
        , valDecl
        , varPattern
        )
import Elm.Pretty
import Extra.GenerateElm exposing (..)
import Extra.Ui as Ui
import File.Download
import Html exposing (Html)
import Html.Attributes
import SyntaxHighlight
import Task
import Time
import Zip
import Zip.Entry


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { from1To16ShownOrFolded :
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
    ( { from1To16ShownOrFolded = Folded
      , argumentsModuleShownOrFolded = Folded
      }
    , Cmd.none
    )


type Msg
    = DownloadModules
    | DownloadModulesAtTime ( Time.Zone, Time.Posix )
    | SwitchVisibleModule ModulesInElmArrs


type ModulesInElmArrs
    = From1To16
    | Arguments


type From1To16Tag
    = From1To16Tag


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
                    [ toZipEntry from1To16
                    , toZipEntry argumentsModule
                    ]
                    |> Zip.toBytes
                )
            )

        SwitchVisibleModule moduleKind ->
            ( case moduleKind of
                From1To16 ->
                    { model
                        | from1To16ShownOrFolded =
                            switchShownOrFolded
                                (.from1To16ShownOrFolded model)
                                viewFrom1To16
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


natNAnn : Elm.CodeGen.TypeAnnotation -> Elm.CodeGen.TypeAnnotation
natNAnn n =
    typed "Nat" [ n ]


nAnn : Int -> Elm.CodeGen.TypeAnnotation
nAnn n =
    typed "N"
        [ natXAnn n
        , natXPlusAnn n (typeVar "more")
        , isAnn n "a"
        , isAnn n "b"
        ]


isAnn : Int -> String -> Elm.CodeGen.TypeAnnotation
isAnn n var =
    typed "Is"
        [ typeVar var
        , toAnn
        , natXPlusAnn n (typeVar var)
        ]


toAnn : Elm.CodeGen.TypeAnnotation
toAnn =
    typed "To" []


natXAnn : Int -> Elm.CodeGen.TypeAnnotation
natXAnn x =
    typed ("Nat" ++ String.fromInt x) []


natXPlusAnn : Int -> Elm.CodeGen.TypeAnnotation -> Elm.CodeGen.TypeAnnotation
natXPlusAnn x more =
    case x of
        0 ->
            more

        _ ->
            typed ("Nat" ++ String.fromInt x ++ "Plus") [ more ]


toIntAnn : Elm.CodeGen.TypeAnnotation
toIntAnn =
    funAnn [ natNAnn (typeVar "n") ] intAnn



--


viewArgumentsModule : Ui.Element msg
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
                    [ varPattern "fun", varPattern "more" ]
                    (lambda [ varPattern "a" ]
                        applyXMinus1
                    )
        in
        applyPreset 1
            (applyBinOp (parens (construct "fun" [ val "a" ]))
                piper
                (val "more")
            )
            :: (List.range 2 16
                    |> List.map
                        (\x ->
                            applyPreset x
                                (construct ("apply" ++ String.fromInt (x - 1))
                                    [ parens (construct "fun" [ val "a" ])
                                    , val "more"
                                    ]
                                )
                        )
               )
    }


viewFrom1To16 : Ui.Element msg
viewFrom1To16 =
    Ui.module_ from1To16


from1To16 : Module From1To16Tag
from1To16 =
    { name = [ "From1To16" ]
    , roleInPackage =
        PackageExposedModule
            { moduleComment = \_ -> []
            }
    , imports =
        []
    , declarations =
        let
            fromXPreset x implementation =
                packageExposedFunDecl From1To16Tag
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
                            [ typed "In" [ natXAnn x, natXPlusAnn x (typeVar "a_") ]
                            , typeVar "element"
                            ]
                        )
                    )
                    ("from" ++ String.fromInt x)
                    []
                    implementation
        in
        fromXPreset 1
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
                                (construct ("apply" ++ String.fromInt (x - 1))
                                    [ fun ("from" ++ String.fromInt (x - 1))
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
    }



--


args : (arg -> String) -> List arg -> String
args argToString =
    List.map argToString >> String.join " "


indexed : ((String -> String) -> a) -> Int -> Int -> List a
indexed use first last =
    List.range first last
        |> List.map
            (\i ->
                use (\base -> base ++ String.fromInt i)
            )


charIndex : Int -> Char
charIndex i =
    i + Char.toCode 'a' |> Char.fromCode


charPrefixed : ((String -> String) -> a) -> Int -> List a
charPrefixed use last =
    List.range 0 last
        |> List.map
            (charIndex >> (\i -> use (String.cons i)))


view : Model -> Html Msg
view { from1To16ShownOrFolded, argumentsModuleShownOrFolded } =
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
                        [ ( from1To16ShownOrFolded
                          , ( "From1To16", From1To16 )
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
