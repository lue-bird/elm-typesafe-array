module Extra.Misc exposing (stringFromBool)

{-| **Should not be exposed.**

Utility functions.

-}


{-| Convert a `Bool` to a `String`: `True` becomes `"True"`, `False` becomes `"False"`.
-}
stringFromBool : Bool -> String
stringFromBool bool =
    case bool of
        True ->
            "True"

        False ->
            "False"
