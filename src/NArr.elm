module NArr exposing (push, extend, removeAt)

{-|


## modify

@docs push, extend, removeAt

-}

import Arr exposing (Arr)
import Internal.NArr as Internal
import LinearDirection exposing (LinearDirection)
import Nat exposing (In, Is, N, Nat, To, ValueN)
import TypeNats exposing (..)



-- ## modify


{-| Put a new element after the others.

    Arr.from5 1 2 3 4 5
        |> Arr.push 6
    --> Arr.from6 1 2 3 4 5 6

-}
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
    Internal.push elementToPush


{-| Append an `Arr N` containing a fixed amount of elements.

    sixEmptySlots =
        Arr.repeat nat6 0

    Arr.from2 1 2
        |> NArr.extend nat6 sixEmptySlots
    --> Arr.from6 1 2 0 0 0 0 0 0

-}
extend :
    Nat (N added (Is n To sum) (Is atLeastN To atLeastSum))
    -> Arr (N added (Is aPlusN To aPlusSum) (Is bPlusN To bPlusSum)) element
    ->
        Arr
            (ValueN
                n
                atLeastN
                (Is a To aPlusN)
                (Is b To bPlusN)
            )
            element
    ->
        Arr
            (ValueN
                sum
                atLeastSum
                (Is a To aPlusSum)
                (Is b To bPlusSum)
            )
            element
extend addedLength extension =
    Internal.extend addedLength extension


{-| Kick an element out of an Arr at a given index in a direction.

    Arr.from3 'a' 'a' 'b'
        |> InArr.removeAt nat1 FirstToLast
    --> Arr.from2 'a' 'b'

    Arr.from4 'a' 'b' 'c' 'd'
        |> InArr.removeAt nat0 LastToFirst
    --> Arr.from3 'a' 'b' 'c'

-}
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
    Internal.removeAt index direction
