module ArrayExtra exposing (allOk)

{-| Should be replaced by Array.Extra functions if they are added there


# transform

@docs allOk

-}

import Array exposing (Array)
import Emptiable exposing (Emptiable, filled)
import Stack exposing (Stacked)


allOk :
    Array (Result error ok)
    ->
        Result
            (Emptiable (Stacked { index : Int, error : error }) Never)
            (Array ok)
allOk =
    \array ->
        array
            |> Array.foldr
                (\element soFar ->
                    { index = soFar.index + 1
                    , combined =
                        case soFar.combined of
                            Ok soFarOks ->
                                case element of
                                    Ok elementOk ->
                                        soFarOks |> (::) elementOk |> Ok

                                    Err elementError ->
                                        Stack.one { index = soFar.index, error = elementError } |> Err

                            Err soFarErrors ->
                                case element of
                                    Ok elementOk ->
                                        soFarErrors |> Err

                                    Err elementError ->
                                        soFarErrors |> Stack.onTopLay { index = soFar.index, error = elementError } |> Err
                    }
                )
                { index = 0, combined = [] |> Ok }
            |> .combined
            |> Result.map Array.fromList
