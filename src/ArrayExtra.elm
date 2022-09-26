module ArrayExtra exposing (allFill)

{-| Should be replaced by Array.Extra functions if they are added there


# transform

@docs allFill

-}

import Array exposing (Array)
import Emptiable exposing (Emptiable, fillAnd, fillMap, filled)


allFill :
    Array (Emptiable element possiblyOrNever)
    -> Emptiable (Array element) possiblyOrNever
allFill =
    \array ->
        array
            |> Array.toList
            |> listAllFill
            |> fillMap Array.fromList


listAllFill :
    List (Emptiable element possiblyOrNever)
    -> Emptiable (List element) possiblyOrNever
listAllFill =
    List.foldr
        (\element soFar ->
            soFar
                |> fillAnd element
                |> fillMap
                    (\( fills, fill ) -> fills |> (::) fill)
        )
        ([] |> filled)
