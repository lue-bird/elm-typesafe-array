module Internal.NArr exposing (extend, push, removeAt, serialize)

import Arr exposing (Arr)
import Array
import Internal.Arr as Internal
import LinearDirection exposing (LinearDirection)
import NNat
import NNats exposing (nat1)
import Nat exposing (In, Is, N, Nat, To, ValueN)
import Serialize
import TypeNats exposing (..)
import Typed exposing (val)


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


{-| Just a draft, will be updated like bounded-nat
-}
serialize :
    Nat length
    -> Serialize.Codec String a
    -> Serialize.Codec String (Arr length a)
serialize length_ serializeElement =
    Serialize.array serializeElement
        |> Typed.serializeChecked Internal.Arr
            (\array ->
                if Array.length array == val length_ then
                    Ok { array = array, length = length_ }

                else
                    Err "The decoded Array wasn't of the expected size"
            )
            .array
