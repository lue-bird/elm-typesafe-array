module ArrayExtra exposing (reverse, whenAllJust, any, all)

{-| Should be replaced by Array.Extra functions if they are added there.
-}

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

any : (a -> Bool) -> Array a -> Bool
any isOkay =
    Array.toList
        >> List.any isOkay

all : (a -> Bool) -> Array a -> Bool
all isOkay =
    Array.toList
        >> List.all isOkay
