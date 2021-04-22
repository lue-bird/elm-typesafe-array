module InArr exposing (push, extend, extendN, removeAt, insertAt)

{-| A `Arr (In ...)` describes an array where you know a minimum & maximum amount of elements.

    Array.empty |> Array.get 0
    --> Nothing

    Arr.empty |> Arr.at nat0 FirstToLast
    --> compile time error

Is this any useful? Let's look at an example:

> 0 to 100 joined by a space

    joinBy between =
        \before after-> before ++ between ++ after

    let
        intStringsAfter =
            Array.fromList (List.range 1 100)
                |> Array.map String.fromInt
    in
    Array.foldl (joinBy " ") "0" intStringsAfter
    --> "0 1 2 3 4 5 ..."

    Arr.nats nat100
        |> Arr.map (val >> String.fromInt)
        |> Arr.foldWithFirst (joinBy " ")
    --> "0 1 2 3 4 5 ..."


## modify

@docs push, extend, extendN, removeAt, insertAt

-}

import Arr exposing (Arr)
import Internal.InArr as Internal
import LinearDirection exposing (LinearDirection(..))
import NNats exposing (..)
import Nat exposing (In, Is, N, Nat, To, ValueIn)
import TypeNats exposing (..)



-- ## modify


{-| Put a new element after the others.

    arrWith5To10Elements
        |> InArr.push "becomes the last"
    --> is of type Arr (ValueIn Nat6 Nat11) String

-}
push :
    element
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn (Nat1Plus min) (Nat1Plus max)) element
push element =
    Internal.push element


{-| Use `MinArr.push`, if the `max` isn't known to be a certain value.
-}
insertAt :
    Nat (In indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minMinus1) max maybeN) element
    -> Arr (ValueIn (Nat2Plus minMinus1) (Nat1Plus max)) element
insertAt index direction element =
    Internal.insertAt index direction element


{-| Append the elements of another `Arr (In ...)`.
-}
extend :
    Arr (In extensionMin extensionMax extensionMaybeN) element
    -> Nat (N extensionMin (Is min To extendedMin) x)
    -> Nat (N extensionMax (Is max To extendedMax) y)
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn extendedMin extendedMax) element
extend extension extensionMin extensionMax =
    Internal.extend extension extensionMin extensionMax


{-| Append the elements of a `Arr (N ...)`.
-}
extendN :
    Arr (N added (Is min To sumMin) (Is max To sumMax)) element
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn sumMin sumMax) element
extendN arrExtension =
    Internal.extendN arrExtension


{-| Kick an element out of an Arr at a given index in a direction.
-}
removeAt :
    Nat (In indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) (Nat1Plus maxMinus1) maybeN) element
    -> Arr (ValueIn minMinus1 maxMinus1) element
removeAt index direction =
    Internal.removeAt index direction
