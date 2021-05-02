module Internal.Arr exposing (ArrTag(..), Content, at, drop, empty, extend, fromArray, groupsOf, insertAt, length, lowerMinLength, map, map2, mapArrayAndLength, mapLength, nats, push, random, removeAt, repeat, replaceAt, restoreMaxLength, reverse, serialize, take, toArray)

{-| ArgOnly use it in `Internal.Arr. ...` modules.
-}

import Array exposing (Array)
import Array.Extra
import Array.LinearDirection as Array
import ArrayExtra as Array
import InNat
import LinearDirection exposing (LinearDirection)
import MinNat
import NNats exposing (..)
import Nat exposing (ArgIn, ArgN, In, Is, Min, Nat, Only, To)
import Random
import Serialize
import TypeNats exposing (..)
import Typed exposing (Checked, Internal, Tagged, Typed, internalVal, internalVal2, isChecked, tag, val)


type alias Arr length element =
    ArrAs Checked length element


type alias ArrAs whoCanCreate length element =
    Typed whoCanCreate ArrTag Internal (Content length element)


type alias Content length element =
    { array : Array element, length : Nat length }


type ArrTag
    = Arr



-- ## scan


at :
    Nat (ArgIn indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) max) element
    -> element
at index direction =
    \arr ->
        case Array.at (val index) direction (toArray arr) of
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
    -> ArrAs Tagged mappedLength mappedElement
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
    -> ArrAs Tagged mappedLength element
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
    -> ArrAs Tagged length mappedElement
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


map2 :
    (aElement -> bElement -> combined)
    -> Arr length aElement
    -> Arr length bElement
    -> Arr length combined
map2 combine aArr bArr =
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


empty : Arr (Only Nat0) element
empty =
    { array = Array.empty
    , length = nat0 |> InNat.value
    }
        |> tag
        |> isChecked Arr


repeat :
    Nat (ArgIn min max maybeN)
    -> element
    -> Arr (In min max) element
repeat amount element =
    { array = Array.repeat (val amount) element
    , length = amount |> InNat.value
    }
        |> tag
        |> isChecked Arr


fromArray : Array element -> Arr (Min Nat0) element
fromArray array =
    { array = array
    , length =
        Array.length array
            |> Nat.intAtLeast (nat0 |> MinNat.value)
    }
        |> tag
        |> isChecked Arr


nats :
    Nat
        (ArgIn (Nat1Plus minLengthMinus1) (Nat1Plus maxLengthMinus1) lengthMaybeN)
    ->
        Arr
            (In (Nat1Plus minLengthMinus1) (Nat1Plus maxLengthMinus1))
            (Nat (In Nat0 maxLengthMinus1))
nats length_ =
    { array =
        List.range 0 (val length_ - 1)
            |> List.map
                (Nat.intInRange nat0 (length_ |> InNat.subN nat1))
            |> Array.fromList
    , length = length_ |> InNat.value
    }
        |> tag
        |> isChecked Arr


random :
    Nat length
    -> Random.Generator element
    -> Random.Generator (Arr length element)
random amount generateElement =
    Random.list (val amount) generateElement
        |> Random.map
            (\list ->
                { array = Array.fromList list, length = amount }
                    |> tag
                    |> isChecked Arr
            )



-- ## modify


replaceAt :
    Nat (ArgIn indexMin minLengthMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minLengthMinus1) max) element
    -> Arr (In (Nat1Plus minLengthMinus1) max) element
replaceAt index direction replacingElement =
    mapArray
        (Array.replaceAt (index |> val) direction replacingElement)
        >> isChecked Arr



-- ## part


take :
    Nat (ArgIn minTaken maxTaken takenMaybeN)
    -> Nat (ArgN maxTaken (Is a To atLeastMaxTaken) (Is maxTakenToMin To min))
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken atLeastMaxTaken) element
take amount maxTakenAmount direction =
    mapArrayAndLength
        (Array.take (amount |> val) direction)
        (\_ ->
            amount
                |> Nat.restoreMax maxTakenAmount
                |> InNat.value
        )
        >> isChecked Arr


drop :
    Nat (ArgN dropped (Is minTaken To min) (Is maxTaken To max))
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken maxTaken) element
drop droppedAmount direction =
    mapArrayAndLength
        (Array.drop (droppedAmount |> val) direction)
        (\len -> len |> InNat.subN droppedAmount)
        >> isChecked Arr



-- ## drop information


lowerMinLength :
    Nat (ArgIn lowerMin min lowerMaybeN)
    -> Arr (In min max) element
    -> Arr (In lowerMin max) element
lowerMinLength validMinimumLength =
    mapLength (Nat.lowerMin validMinimumLength)
        >> isChecked Arr


restoreMaxLength :
    Nat (ArgN max (Is a To atLeastMax) x)
    -> Arr (In min max) element
    -> Arr (In min atLeastMax) element
restoreMaxLength maximumLength =
    mapLength (Nat.restoreMax maximumLength)
        >> isChecked Arr


reverse : Arr length element -> Arr length element
reverse =
    mapArray Array.reverse >> isChecked Arr



-- ## extra


serialize :
    Nat (ArgIn min max maybeN)
    -> Serialize.Codec String element
    -> Serialize.Codec String (Arr (In min max) element)
serialize length_ serializeElement =
    Serialize.array serializeElement
        |> Serialize.mapValid
            (\array ->
                if Array.length array == val length_ then
                    { array = array
                    , length = length_ |> InNat.value
                    }
                        |> tag
                        |> isChecked Arr
                        |> Ok

                else
                    "Array length was different from the required length "
                        ++ String.fromInt (val length_)
                        |> Err
            )
            toArray



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
    Nat (ArgIn minIndex minLengthMinus1 indexMaybeN)
    -> LinearDirection
    ->
        (Nat (In (Nat1Plus minLengthMinus1) maxLength)
         -> Nat resultLengthMinus1
        )
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength) element
    -> Arr resultLengthMinus1 element
removeAt index direction sub1 =
    mapArrayAndLength
        (Array.removeAt (val index) direction)
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
        (Array.insertAt (index |> val) direction inserted)
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


groupsOf :
    Nat (ArgIn (Nat1Plus minGroupSizMinus1) maxGroupSize groupSizeMaybeN)
    -> LinearDirection
    -> Arr (In min max) element
    ->
        { groups :
            Arr
                (In Nat0 max)
                (Arr
                    (ArgIn (Nat1Plus minGroupSizMinus1) maxGroupSize groupSizeMaybeN)
                    element
                )
        , less : Arr (In Nat0 maxGroupSize) element
        }
groupsOf groupSize direction =
    \arr ->
        let
            { groups, less } =
                toArray arr
                    |> Array.group (val groupSize) direction
        in
        { groups =
            { array =
                groups
                    |> Array.map
                        (\array ->
                            { array = array, length = groupSize }
                                |> tag
                                |> isChecked Arr
                        )
            , length = length arr |> Nat.div groupSize
            }
                |> tag
                |> isChecked Arr
        , less =
            { array = less
            , length = length arr |> Nat.remainderBy groupSize
            }
                |> tag
                |> isChecked Arr
        }
