module ArrayExtra exposing
    ( natLength
    , whenAllJust
    )

{-| Should be replaced by Array.Extra functions if they are added there.


# scan

@docs natLength


# modify

@docs reverse, intersperse


# transform

@docs whenAllJust

-}

import Array exposing (Array)
import Nat exposing (Min, Nat)
import Nats exposing (Nat0, nat0)


whenAllJustInList : List (Maybe a) -> Maybe (List a)
whenAllJustInList =
    List.foldr (Maybe.map2 (::)) (Just [])


whenAllJust : Array (Maybe a) -> Maybe (Array a)
whenAllJust maybes =
    maybes
        |> Array.toList
        |> whenAllJustInList
        |> Maybe.map Array.fromList


natLength : Array a_ -> Nat (Min Nat0)
natLength =
    Array.length >> Nat.intAtLeast nat0
