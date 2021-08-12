module ArrayExtra exposing
    ( any, all, natLength
    , reverse, intersperse
    , whenAllJust
    )

{-| Should be replaced by Array.Extra functions if they are added there.


# scan

@docs any, all, natLength


# modify

@docs reverse, intersperse


# transform

@docs whenAllJust

-}

import Array exposing (Array)
import Nat exposing (Min, Nat)
import Nats exposing (Nat0, nat0)


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


natLength : Array a_ -> Nat (Min Nat0)
natLength =
    Array.length >> Nat.intAtLeast nat0


intersperse : a -> Array a -> Array a
intersperse separator =
    Array.toList
        >> List.intersperse separator
        >> Array.fromList
