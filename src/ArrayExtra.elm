module ArrayExtra exposing (allOk)

{-| Should be replaced by Array.Extra functions if they are added there


# transform

@docs allOk

-}

import Array exposing (Array)
import Emptiable exposing (Emptiable)
import Stack exposing (Stacked)


allOk :
    Array (Result error ok)
    ->
        Result
            (Emptiable (Stacked error) Never)
            (Array ok)
allOk =
    \array ->
        array
            |> Array.foldr
                (\element soFar ->
                    case soFar of
                        Ok soFarOks ->
                            case element of
                                Ok elementOk ->
                                    soFarOks |> (::) elementOk |> Ok

                                Err elementError ->
                                    Stack.one elementError |> Err

                        Err soFarErrors ->
                            case element of
                                Ok _ ->
                                    soFarErrors |> Err

                                Err elementError ->
                                    soFarErrors |> Stack.onTopLay elementError |> Err
                )
                ([] |> Ok)
            |> Result.map Array.fromList
