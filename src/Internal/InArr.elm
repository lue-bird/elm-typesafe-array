module Internal.InArr exposing
    ( dropN
    , extend
    , extendN
    , insertAt
    , push
    , removeAt
    , toMin
    )

{-| All functions must be tested a lot, especially the type signatures.
Try to reduce the amount of functions.
-}

import Arr exposing (Arr, length)
import InNat
import Internal.Arr as Internal
import LinearDirection exposing (LinearDirection)
import NNats exposing (..)
import Nat exposing (Nat)
import Nat.Bound exposing (..)
import TypeNats exposing (..)


toMin :
    Arr (In min max maybeN) element
    -> Arr (ValueMin min) element
toMin =
    Internal.mapLength InNat.toMin


push :
    element
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn (Nat1Plus min) (Nat1Plus max)) element
push element =
    Internal.push element (InNat.addN nat1)


insertAt :
    Nat (In indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minMinus1) max maybeN) element
    -> Arr (ValueIn (Nat2Plus minMinus1) (Nat1Plus max)) element
insertAt index direction insertedElement =
    Internal.insertAt index
        direction
        insertedElement
        (InNat.addN nat1)


removeAt :
    Nat (In indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) (Nat1Plus maxMinus1) maybeN) element
    -> Arr (ValueIn minMinus1 maxMinus1) element
removeAt index direction =
    Internal.removeAt index direction (InNat.subN nat1)


extend :
    Arr (In extensionMin extensionMax extensionMaybeN) element
    -> Nat (N extensionMin (Is min To) extendedMin x)
    -> Nat (N extensionMax (Is max To) extendedMax y)
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn extendedMin extendedMax) element
extend extension extensionMin extensionMax =
    Internal.extend extension
        (\extensionLen ->
            InNat.add extensionLen extensionMin extensionMax
        )


extendN :
    Arr (N added (Is min To) sumMin (And max To sumMax)) element
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn sumMin sumMax) element
extendN nArrExtension =
    Internal.extend nArrExtension InNat.addN


dropN :
    Nat (N dropped (Is minTaken To) min (And maxTaken To max))
    -> LinearDirection
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn minTaken maxTaken) element
dropN droppedAmount direction =
    \arr ->
        arr
            |> Internal.take
                (length arr |> InNat.subN droppedAmount)
                (LinearDirection.opposite direction)
