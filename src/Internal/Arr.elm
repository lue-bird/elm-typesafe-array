module Internal.Arr exposing (ArrTag(..), Value, at, combine2, drop, empty, extend, fromArray, insertAt, length, map, mapArrayAndLength, mapLength, nPush, nats, push, random, removeAt, repeat, replaceAt, reverse, take, toArray)

{-| Only use it in `Internal.Arr. ...` modules.
-}

import Array exposing (Array)
import Array.Extra
import ArrayExtra as Array
import InNat
import LinearDirection exposing (LinearDirection)
import LinearDirection.Array as Array
import MinNat
import NNat
import NNats exposing (..)
import Nat exposing (Nat)
import Nat.Bound exposing (..)
import Random
import TypeNats exposing (..)
import Typed exposing (Checked, Internal, Tagged, Typed, internalVal, internalVal2, isChecked, tag)


type alias Arr length element =
    Typed Checked ArrTag Internal (Value length element)


type alias Value length element =
    { array : Array element, length : Nat length }


type ArrTag
    = Arr



-- ## scan


at :
    Nat (In indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) max maybeN) element
    -> element
at index direction =
    \arr ->
        case Array.at (Nat.toInt index) direction (toArray arr) of
            Just element ->
                --succeeds for every correct typed Arr (should be)
                element

            Nothing ->
                --if not, we crash with a
                --RangeError: Maximum call stack size exceeded
                at index direction arr


length : Arr length element -> Nat length
length =
    internalVal Arr >> .length



-- ## transform


toArray : Arr length element -> Array element
toArray =
    internalVal Arr >> .array


mapArrayAndLength :
    (Array element -> Array mappedElement)
    -> (Nat length -> Nat mappedLength)
    -> Arr length element
    -> Typed Tagged ArrTag Internal (Value mappedLength mappedElement)
mapArrayAndLength mapArray_ mapLength_ =
    Typed.map
        (\v ->
            { array = mapArray_ v.array
            , length = mapLength_ v.length
            }
        )


mapLength :
    (Nat length -> Nat mappedLength)
    -> Arr length element
    -> Typed Tagged ArrTag Internal (Value mappedLength element)
mapLength mapLength_ =
    Typed.map
        (\v ->
            { array = v.array
            , length = mapLength_ v.length
            }
        )


mapArray :
    (Array element -> Array mappedElement)
    -> Arr length element
    -> Typed Tagged ArrTag Internal (Value length mappedElement)
mapArray mapArray_ =
    Typed.map
        (\v ->
            { length = v.length
            , array = mapArray_ v.array
            }
        )


map :
    (aElement -> bElement)
    -> Arr length aElement
    -> Arr length bElement
map alter =
    mapArray (Array.map alter)
        >> isChecked Arr


combine2 :
    (a -> b -> combined)
    -> Arr length a
    -> Arr length b
    -> Arr length combined
combine2 combine aArr bArr =
    let
        map2Val a b =
            { array =
                Array.Extra.map2 combine a.array b.array
            , length =
                Nat.theSmaller a.length b.length
            }
    in
    internalVal2 map2Val Arr aArr Arr bArr
        |> tag
        |> isChecked Arr



-- ## create


empty : Arr (ValueN Nat0 atLeast0 (Is a To a) (Is b To b)) element
empty =
    tag { array = Array.empty, length = nat0 }
        |> isChecked Arr


repeat : Nat amount -> element -> Arr amount element
repeat amount element =
    { array = Array.repeat (Nat.toInt amount) element
    , length = amount
    }
        |> tag
        |> isChecked Arr


fromArray : Array element -> Arr (ValueMin Nat0) element
fromArray array =
    { array = array
    , length =
        Array.length array
            |> Nat.intAtLeast (nat0 |> Nat.toMin)
    }
        |> tag
        |> isChecked Arr


nats :
    Nat
        (In (Nat1Plus minLengthMinus1) (Nat1Plus maxLengthMinus1) lengthMaybeN)
    ->
        Arr
            (In (Nat1Plus minLengthMinus1) (Nat1Plus maxLengthMinus1) lengthMaybeN)
            (Nat (ValueIn Nat0 maxLengthMinus1))
nats length_ =
    { array =
        List.range 0 (Nat.toInt length_ - 1)
            |> List.map
                (Nat.intInRange nat0 (length_ |> InNat.subN nat1))
            |> Array.fromList
    , length = length_
    }
        |> tag
        |> isChecked Arr


random :
    Nat length
    -> Random.Generator element
    -> Random.Generator (Arr length element)
random amount generateElement =
    Random.list (Nat.toInt amount) generateElement
        |> Random.map
            (\list ->
                { array = Array.fromList list, length = amount }
                    |> tag
                    |> isChecked Arr
            )



-- ## modify


nPush :
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
nPush elementToPush =
    push elementToPush (NNat.add ( nat1, nat1 ))


replaceAt :
    Nat (In indexMin minLengthMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minLengthMinus1) max maybeN) element
    -> Arr (In (Nat1Plus minLengthMinus1) max maybeN) element
replaceAt index direction replacingElement =
    mapArray
        (Array.replaceAt (index |> Nat.toInt) direction replacingElement)
        >> isChecked Arr



-- ## part


take :
    Nat (In minTaken maxTaken takenMaybeN)
    -> Nat (N maxTaken (Is a To atLeastMaxTaken) (Is maxTakenToMin To min))
    -> LinearDirection
    -> Arr (In min max maybeN) element
    -> Arr (In minTaken atLeastMaxTaken takenMaybeN) element
take amount maxTakenAmount direction =
    mapArrayAndLength
        (Array.take (amount |> Nat.toInt) direction)
        (\_ -> amount |> Nat.maxIs maxTakenAmount)
        >> isChecked Arr


drop :
    Nat (N dropped (Is minTaken To min) (Is maxTaken To max))
    -> LinearDirection
    -> Arr (In min max maybeN) element
    -> Arr (ValueIn minTaken maxTaken) element
drop droppedAmount direction =
    mapArrayAndLength
        (Array.drop (droppedAmount |> Nat.toInt) direction)
        (\len -> len |> InNat.subN droppedAmount)
        >> isChecked Arr



-- ## extra


reverse : Arr length element -> Arr length element
reverse =
    mapArray Array.reverse >> isChecked Arr



-- presets


extend :
    Arr addedLength element
    -> (Nat addedLength -> Nat length -> Nat lengthSum)
    -> Arr length element
    -> Arr lengthSum element
extend extension addLength =
    let
        appendVal a b =
            { array = Array.append b.array a.array
            , length = addLength a.length b.length
            }
    in
    internalVal2 appendVal Arr extension Arr
        >> tag
        >> isChecked Arr


removeAt :
    Nat (In minIndex minLengthMinus1 indexMaybeN)
    -> LinearDirection
    ->
        (Nat (In (Nat1Plus minLengthMinus1) maxLength maybeN)
         -> Nat resultLengthMinus1
        )
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength maybeN) element
    -> Arr resultLengthMinus1 element
removeAt index direction sub1 =
    mapArrayAndLength
        (Array.removeAt (Nat.toInt index) direction)
        sub1
        >> isChecked Arr


insertAt :
    Nat range
    -> LinearDirection
    -> element
    -> (Nat length -> Nat lengthPlus1)
    -> Arr length element
    -> Arr lengthPlus1 element
insertAt index direction inserted add1 =
    mapArrayAndLength
        (Array.insertAt (index |> Nat.toInt) direction inserted)
        add1
        >> isChecked Arr


push :
    element
    -> (Nat length -> Nat lengthPlus1)
    -> Arr length element
    -> Arr lengthPlus1 element
push elementToPush add1 =
    mapArrayAndLength (Array.push elementToPush) add1
        >> isChecked Arr
