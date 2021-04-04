module ArrayExtra exposing (reverse)

import Array exposing (Array)


{-| Do you know a better way?
-}
reverse : Array a -> Array a
reverse =
    Array.toList >> List.reverse >> Array.fromList
