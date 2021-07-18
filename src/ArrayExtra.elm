module ArrayExtra exposing (reverse, whenAllJust)

import Array exposing (Array)


reverse : Array a -> Array a
reverse =
    Array.toList >> List.reverse >> Array.fromList


whenAllJustInList : List (Maybe a) -> Maybe (List a)
whenAllJustInList =
    List.foldr (Maybe.map2 (::)) (Just [])


whenAllJust : Array (Maybe a) -> Maybe (Array a)
whenAllJust maybes =
    maybes
        |> Array.toList
        |> whenAllJustInList
        |> Maybe.map Array.fromList
