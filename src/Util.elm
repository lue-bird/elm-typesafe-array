module Util exposing (filledToOk)

import Emptiable exposing (Emptiable)


filledToOk : Emptiable value error -> Result error value
filledToOk =
    \emptiable ->
        case emptiable of
            Emptiable.Filled fill ->
                fill |> Ok

            Emptiable.Empty possiblyOrNever ->
                possiblyOrNever |> Err
