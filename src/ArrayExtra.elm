module ArrayExtra exposing
    ( lengthN
    , interweave
    , allFill
    )

{-| Should be replaced by Array.Extra functions if they are added there.


# scan

@docs lengthN


# alter

@docs interweave


# transform

@docs allFill

-}

import Array exposing (Array)
import Emptiable exposing (Emptiable, fillMap, filled)
import N exposing (Min, N, To, Up, n0)


allFill :
    Array (Emptiable element possiblyOrNever)
    -> Emptiable (Array element) possiblyOrNever
allFill maybes =
    maybes
        |> Array.toList
        |> areAllFilledInList
        |> fillMap Array.fromList


areAllFilledInList :
    List (Emptiable element possiblyOrNever)
    -> Emptiable (List element) possiblyOrNever
areAllFilledInList =
    List.foldr
        (\element ->
            Emptiable.fillAnd element
                >> Emptiable.fillMap
                    (\( fills, fill ) -> fills |> (::) fill)
        )
        ([] |> filled)


lengthN : Array element_ -> N (Min (Up x To x))
lengthN =
    Array.length >> N.intAtLeast n0


{-| TOREPLACE once <https://github.com/elm-community/array-extra/pull/30> goes through
-}
interweave : Array element -> (Array element -> Array element)
interweave toInterweave =
    \array ->
        let
            untilArrayEnd =
                array
                    |> Array.foldl
                        (\element soFar ->
                            case soFar.toInterweave of
                                [] ->
                                    { interwoven =
                                        element :: soFar.interwoven
                                    , toInterweave = []
                                    }

                                toInterweaveHead :: toInterweaveTail ->
                                    { interwoven =
                                        toInterweaveHead
                                            :: element
                                            :: soFar.interwoven
                                    , toInterweave = toInterweaveTail
                                    }
                        )
                        { interwoven = []
                        , toInterweave = toInterweave |> Array.toList
                        }
        in
        (untilArrayEnd.interwoven
            |> List.reverse
        )
            ++ untilArrayEnd.toInterweave
            |> Array.fromList
