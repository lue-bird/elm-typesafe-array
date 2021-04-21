module Extra.GenerateElm exposing (..)

{-| Content to create a `Generate.file`.
-}

import Bytes.Encode
import Elm.CodeGen as Generate
import Elm.Pretty as Pretty
import Time
import Zip
import Zip.Entry


{-| Content to create a `Generate.file`.
-}
type alias Module tag =
    { name : Generate.ModuleName
    , roleInPackage : ModuleRoleInPackage tag
    , imports : List Generate.Import
    , declarations : List (Declaration tag)
    }


type alias PackageInternalModule =
    Module Never


docTagsFrom :
    tag
    -> List (Declaration tag)
    -> Generate.Comment Generate.FileComment
    -> Generate.Comment Generate.FileComment
docTagsFrom tag declarations =
    Generate.docTagsFromExposings
        (declarations
            |> List.filterMap
                (\(Declaration decl) ->
                    decl.exposedOrLocal
                        |> Maybe.andThen
                            (\{ makeExposing, maybeTag } ->
                                maybeTag
                                    |> Maybe.map
                                        (\declTag ->
                                            { expose = makeExposing (.name decl), tag = declTag }
                                        )
                            )
                )
            |> List.filter (.tag >> (==) tag)
            |> List.map .expose
        )


noAlias : Maybe Generate.ModuleName
noAlias =
    Nothing


importAlias : String -> Maybe Generate.ModuleName
importAlias aliasName =
    Just [ aliasName ]


noExposing : Maybe Generate.Exposing
noExposing =
    Nothing


exposingExplicit :
    List Generate.TopLevelExpose
    -> Maybe Generate.Exposing
exposingExplicit exposings =
    Just (Generate.exposeExplicit exposings)


exposingAll : Maybe Generate.Exposing
exposingAll =
    Just Generate.exposeAll


type ModuleRoleInPackage tag
    = PackageExposedModule
        { moduleComment :
            List (Declaration tag)
            -> Comment Generate.FileComment
        }
    | PackageInternalModule


zipEntryFromModule : ( Time.Zone, Time.Posix ) -> Module tag -> Zip.Entry.Entry
zipEntryFromModule time moduleFile =
    stringFromModuleFile moduleFile
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> Zip.Entry.store
            { path = (.name moduleFile |> String.join "/") ++ ".elm"
            , lastModified = time
            , comment = Nothing
            }


type alias Comment comment =
    List
        (Generate.Comment comment
         -> Generate.Comment comment
        )


toDocComment : Comment Generate.DocComment -> Generate.Comment Generate.DocComment
toDocComment =
    List.foldl (<|) Generate.emptyDocComment


toModuleComment : Comment Generate.FileComment -> Generate.Comment Generate.FileComment
toModuleComment =
    List.foldl (<|) Generate.emptyFileComment



--


toDeclaration : Declaration tag -> Generate.Declaration
toDeclaration (Declaration declaration) =
    declaration.make declaration.name


type Declaration tag
    = Declaration
        { make : String -> Generate.Declaration
        , name : String
        , exposedOrLocal :
            Maybe
                { makeExposing : String -> Generate.TopLevelExpose
                , maybeTag : Maybe tag
                }
        }


type alias PackageInternalDeclaration =
    Declaration Never


type PackageInternal
    = PackageInternal


type PackageExposed
    = PackageExposed


exposedToJust :
    a
    -> ExposedOrLocal tag
    -> Maybe { makeExposing : a, maybeTag : Maybe tag }
exposedToJust exposeIfJust exposedOrLocal =
    case exposedOrLocal of
        Local ->
            Nothing

        Exposed tag ->
            Just { makeExposing = exposeIfJust, maybeTag = tag }


type ExposedOrLocal tag
    = Exposed (Maybe tag)
    | Local


type TypeConstructorExposed
    = OpenType
    | ClosedType



--


funDeclaration :
    ExposedOrLocal tag
    -> Maybe (Comment Generate.DocComment)
    -> Generate.TypeAnnotation
    -> String
    -> List Generate.Pattern
    -> Generate.Expression
    -> Declaration tag
funDeclaration exposedOrLocal comment typeAnn name argumentPatterns expression =
    { name = name
    , make =
        \name_ ->
            Generate.funDecl
                (comment |> Maybe.map toDocComment)
                (Just typeAnn)
                name_
                argumentPatterns
                expression
    , exposedOrLocal =
        exposedOrLocal |> exposedToJust Generate.funExpose
    }
        |> Declaration


packageExposedFunDecl :
    tag
    -> Comment Generate.DocComment
    -> Generate.TypeAnnotation
    -> String
    -> List String
    -> Generate.Expression
    -> Declaration tag
packageExposedFunDecl tag comment typeAnn name argumentNames expression =
    funDeclaration (Exposed (Just tag))
        (Just comment)
        typeAnn
        name
        (argumentNames |> List.map Generate.varPattern)
        expression


localFunDecl :
    Generate.TypeAnnotation
    -> String
    -> List Generate.Pattern
    -> Generate.Expression
    -> Declaration tag
localFunDecl typeAnn name argumentPatterns expression =
    funDeclaration Local Nothing typeAnn name argumentPatterns expression


packageInternalExposedFunDecl :
    Generate.TypeAnnotation
    -> String
    -> List Generate.Pattern
    -> Generate.Expression
    -> PackageInternalDeclaration
packageInternalExposedFunDecl typeAnn name argumentPatterns expression =
    funDeclaration (Exposed Nothing) Nothing typeAnn name argumentPatterns expression


aliasDeclaration :
    ExposedOrLocal tag
    -> Maybe (Comment Generate.DocComment)
    -> String
    -> List String
    -> Generate.TypeAnnotation
    -> Declaration tag
aliasDeclaration exposedOrLocal comment name arguments annotation =
    { name = name
    , make =
        \name_ ->
            Generate.aliasDecl
                (comment |> Maybe.map toDocComment)
                name_
                arguments
                annotation
    , exposedOrLocal =
        exposedOrLocal |> exposedToJust Generate.typeOrAliasExpose
    }
        |> Declaration


packageExposedAliasDecl :
    tag
    -> Comment Generate.DocComment
    -> String
    -> List String
    -> Generate.TypeAnnotation
    -> Declaration tag
packageExposedAliasDecl tag comment name arguments annotation =
    aliasDeclaration (Exposed (Just tag))
        (Just comment)
        name
        arguments
        annotation


localAliasDecl :
    String
    -> List String
    -> Generate.TypeAnnotation
    -> PackageInternalDeclaration
localAliasDecl name arguments annotation =
    aliasDeclaration Local
        Nothing
        name
        arguments
        annotation


packageInternalExposedAliasDecl :
    String
    -> List String
    -> Generate.TypeAnnotation
    -> PackageInternalDeclaration
packageInternalExposedAliasDecl name arguments annotation =
    aliasDeclaration (Exposed Nothing)
        Nothing
        name
        arguments
        annotation


typeDecl :
    Maybe ( TypeConstructorExposed, Maybe tag )
    -> Maybe (Comment Generate.DocComment)
    -> String
    -> List String
    -> List ( String, List Generate.TypeAnnotation )
    -> Declaration tag
typeDecl exposedOrLocal comment name argumentNames choiceConstructors =
    { name = name
    , make =
        \name_ ->
            Generate.customTypeDecl
                (comment |> Maybe.map toDocComment)
                name
                argumentNames
                choiceConstructors
    , exposedOrLocal =
        exposedOrLocal
            |> Maybe.map
                (\( openOrClosed, maybeTag ) ->
                    { maybeTag = maybeTag
                    , makeExposing =
                        case openOrClosed of
                            ClosedType ->
                                Generate.closedTypeExpose

                            OpenType ->
                                Generate.openTypeExpose
                    }
                )
    }
        |> Declaration


packageExposedTypeDecl :
    tag
    -> TypeConstructorExposed
    -> Comment Generate.DocComment
    -> String
    -> List String
    -> List ( String, List Generate.TypeAnnotation )
    -> Declaration tag
packageExposedTypeDecl tag constructorExposed comment name argumentNames choiceConstructors =
    typeDecl (Just ( constructorExposed, Just tag ))
        (Just comment)
        name
        argumentNames
        choiceConstructors


packageInternalExposedTypeDecl :
    TypeConstructorExposed
    -> String
    -> List String
    -> List ( String, List Generate.TypeAnnotation )
    -> PackageInternalDeclaration
packageInternalExposedTypeDecl constructorExposed name argumentNames choiceConstructors =
    typeDecl (Just ( constructorExposed, Nothing ))
        Nothing
        name
        argumentNames
        choiceConstructors


localTypeDecl :
    String
    -> List String
    -> List ( String, List Generate.TypeAnnotation )
    -> Declaration tag
localTypeDecl name argumentNames choiceConstructors =
    typeDecl Nothing
        Nothing
        name
        argumentNames
        choiceConstructors



--


stringFromModuleFile : Module tag -> String
stringFromModuleFile moduleFile =
    let
        fromModuleComment moduleComment =
            let
                unpackedDecls =
                    .declarations moduleFile
                        |> List.map (\(Declaration decl) -> decl)

                decls =
                    unpackedDecls
                        |> List.map (\decl -> decl.make decl.name)

                exposings =
                    unpackedDecls
                        |> List.filterMap
                            (\{ exposedOrLocal, name } ->
                                exposedOrLocal
                                    |> Maybe.map (\{ makeExposing } -> makeExposing name)
                            )
            in
            Generate.file
                (Generate.normalModule moduleFile.name exposings)
                moduleFile.imports
                decls
                moduleComment
    in
    case .roleInPackage moduleFile of
        PackageExposedModule { moduleComment } ->
            Pretty.pretty 64
                (fromModuleComment
                    (toModuleComment
                        (moduleComment (.declarations moduleFile))
                        |> Just
                    )
                )

        PackageInternalModule ->
            Pretty.pretty 5000
                (fromModuleComment Nothing)



--


arrayAnn : Generate.TypeAnnotation -> Generate.TypeAnnotation
arrayAnn element =
    Generate.typed "Array" [ element ]


funAnn :
    List Generate.TypeAnnotation
    -> Generate.TypeAnnotation
    -> Generate.TypeAnnotation
funAnn parameters result =
    parameters
        |> List.foldr Generate.funAnn
            result
