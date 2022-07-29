module ArrayExtra exposing
    ( lengthN
    , allFill
    )

{-| Should be replaced by Array.Extra functions if they are added there.


# scan

@docs lengthN


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
