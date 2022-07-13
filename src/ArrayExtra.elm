module ArrayExtra exposing
    ( lengthN
    , areAllFilled
    )

{-| Should be replaced by Array.Extra functions if they are added there.


# scan

@docs lengthN


# transform

@docs areAllFilled

-}

import Array exposing (Array)
import Emptiable exposing (Emptiable, fillMap, filled)
import Linear exposing (DirectionLinear(..))
import N exposing (Min, N, N0, n0)


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


areAllFilled :
    Array (Emptiable element possiblyOrNever)
    -> Emptiable (Array element) possiblyOrNever
areAllFilled maybes =
    maybes
        |> Array.toList
        |> areAllFilledInList
        |> fillMap Array.fromList


lengthN : Array element_ -> N (Min N0)
lengthN =
    Array.length >> N.intAtLeast n0
