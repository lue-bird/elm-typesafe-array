module Internal.NArr exposing (extend, push, removeAt, toIn)

import Arr exposing (Arr)
import Internal.Arr as Internal exposing (mapLength)
import LinearDirection exposing (LinearDirection)
import NNat
import NNats exposing (nat1)
import Nat exposing (In, Is, N, Nat, To, ValueIn, ValueN)
import TypeNats exposing (..)
import Typed exposing (isChecked)


toIn :
    Arr (In min max maybeExact) element
    -> Arr (ValueIn min max) element
toIn =
    mapLength NNat.toIn >> isChecked Internal.Arr


push :
    element
    -> Arr (ValueN n atLeastN (Is a To nPlusA) (Is b To nPlusB)) element
    ->
        Arr
            (ValueN
                (Nat1Plus n)
                (Nat1Plus atLeastN)
                (Is a To (Nat1Plus nPlusA))
                (Is b To (Nat1Plus nPlusB))
            )
            element
push elementToPush =
    Internal.push elementToPush (NNat.add ( nat1, nat1 ))


extend :
    Nat (N addedN (Is n To sumN) (Is atLeastN To atLeastSumN))
    ->
        Arr
            (N
                addedN
                (Is aPlusN To aPlusSum)
                (Is bPlusN To bPlusSum)
            )
            element
    -> Arr (ValueN n atLeastN (Is a To aPlusN) (Is b To bPlusN)) element
    -> Arr (ValueN sumN atLeastSumN (Is a To aPlusSum) (Is b To bPlusSum)) element
extend addedLength extension =
    Internal.extend extension
        (\added len -> len |> NNat.add ( addedLength, added ))


removeAt :
    Nat (In minIndex lengthMinus1 indexMaybeN)
    -> LinearDirection
    ->
        Arr
            (ValueN
                (Nat1Plus lengthMinus1)
                (Nat1Plus atLeastLength)
                (Is a To (Nat1Plus lengthPlusA))
                (Is b To (Nat1Plus lengthPlusB))
            )
            element
    ->
        Arr
            (ValueN
                lengthMinus1
                atLeastLength
                (Is a To lengthPlusA)
                (Is b To lengthPlusB)
            )
            element
removeAt index direction =
    Internal.removeAt index
        direction
        (NNat.sub ( nat1, nat1 ))
