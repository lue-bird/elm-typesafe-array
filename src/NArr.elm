module NArr exposing
    ( toIn
    , push, extend
    )

{-|


## drop information

@docs toIn


## modify

@docs push, extend

-}

import Arr exposing (Arr)
import Internal.NArr as Internal
import Nat exposing (Nat)
import Nat.Bound exposing (..)
import TypeNats exposing (..)



-- ## modify


push :
    element
    -> Arr (N n (Is a To) nPlusA (And b To nPlusB)) element
    ->
        Arr
            (N
                (Nat1Plus n)
                (Is a To)
                (Nat1Plus nPlusA)
                (And b To (Nat1Plus nPlusB))
            )
            element
push elementToPush =
    Internal.push elementToPush


extend :
    Nat (N addedN (Is n To) sumN (And atLeastN To atLeastSumN))
    -> Arr (N addedN (Is aPlusN To) aPlusSum (And bPlusN To bPlusSum)) element
    -> Arr (N n (Is a To) aPlusN (And b To bPlusN)) element
    -> Arr (N sumN (Is a To) aPlusSum (And b To bPlusSum)) element
extend addedLength extension =
    Internal.extend addedLength extension



-- ## drop information


toIn :
    Arr (In min max maybeExact) element
    -> Arr (ValueIn min max) element
toIn =
    Internal.toIn
