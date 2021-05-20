module Internal exposing
    ( empty, fromArray, repeat, nats, minNats, random
    , at, length
    , inIsLengthInRange, inIsLength, inIsLengthAtLeast, inIsLengthAtMost, minIsLength, minIsLengthAtLeast, minIsLengthAtMost
    , toArray, map, map2, reverse
    , replaceAt, inPush, minPush, inInsertAt, minInsertAt, inRemoveAt, minRemoveAt, inExtend, extendIn, minExtend, inDrop, minDrop
    , take, takeMax, groupsOf
    , serialize, serializeIn, serializeMin
    , ArrTag, Content, lowerMinLength, minValue, resize, restoreMaxLength
    )

{-| Only use it in `Internal.Arr. ...` modules.


## create

@docs empty, fromArray, repeat, nats, minNats, random


## scan

@docs at, length


### scan length

@docs inIsLengthInRange, inIsLength, inIsLengthAtLeast, inIsLengthAtMost, minIsLength, minIsLengthAtLeast, minIsLengthAtMost


## transform

@docs toArray, map, map2, reverse


## modify

@docs replaceAt, inPush, minPush, inInsertAt, minInsertAt, inRemoveAt, minRemoveAt, inExtend, extendIn, minExtend, inDrop, minDrop


## part

@docs take, takeMax, groupsOf


## extra

@docs serialize, serializeIn, serializeMin

-}

import Array exposing (Array)
import Array.Extra
import Array.LinearDirection as Array
import ArrayExtra as Array
import InNat
import LinearDirection exposing (LinearDirection(..))
import List.LinearDirection as List
import MinNat
import NNats exposing (..)
import Nat exposing (ArgIn, In, Is, Min, N, Nat, Only, To)
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


{-| **Constructor should not be exposed.**
-}
type ArrTag
    = Arr



-- ## scan


at :
    Nat (ArgIn indexMin minMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) max) element
    -> element
at index direction =
    \arr ->
        case Array.at (val index) direction (toArray arr) of
            Just element ->
                -- succeeds for every correctly typed Arr
                element

            Nothing ->
                -- if not, we crash with a
                -- RangeError: Maximum call stack size exceeded
                -- instead of failing silently like Orasund's static-array does
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
    -> (Nat (In min max) -> Nat (ArgIn mappedMin mappedMax mappedIfN_))
    -> Arr (In min max) element
    -> ArrAs Tagged (In mappedMin mappedMax) mappedElement
mapArrayAndLength mapArray_ mapLength_ =
    Typed.map
        (\v ->
            { array = mapArray_ v.array
            , length = mapLength_ v.length |> InNat.value
            }
        )


mapLength :
    (Nat (In min max) -> Nat (ArgIn mappedMin mappedMax mappedIfN_))
    -> Arr (In min max) element
    -> ArrAs Tagged (In mappedMin mappedMax) element
mapLength mapLength_ =
    Typed.map
        (\v ->
            { array = v.array
            , length = mapLength_ v.length |> InNat.value
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


reverse : Arr length element -> Arr length element
reverse =
    mapArray Array.reverse >> isChecked Arr



-- ## create


empty : Arr (In Nat0 atLeast0) element
empty =
    { array = Array.empty
    , length = nat0 |> InNat.value
    }
        |> tag
        |> isChecked Arr


repeat :
    Nat (ArgIn min max ifN_)
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
            |> Nat.intAtLeast nat0
    }
        |> tag
        |> isChecked Arr


nats :
    Nat (ArgIn minLength (Nat1Plus maxLengthMinus1) lengthIfN_)
    ->
        Arr
            (In minLength (Nat1Plus maxLengthMinus1))
            (Nat (In Nat0 maxLengthMinus1))
nats length_ =
    { array =
        case length_ |> InNat.isAtLeast nat1 { lowest = nat0 } of
            Nat.EqualOrGreater lengthAtLeast1 ->
                List.range 0 (val length_ - 1)
                    |> List.map
                        (Nat.intInRange nat0 (lengthAtLeast1 |> InNat.sub nat1))
                    |> Array.fromList

            Nat.Below _ ->
                Array.empty
    , length = length_ |> InNat.value
    }
        |> tag
        |> isChecked Arr


minNats :
    Nat (ArgIn minLength maxLength lengthIfN_)
    ->
        Arr
            (In minLength maxLength)
            (Nat (In Nat0 maxLength))
minNats length_ =
    { array =
        Nat.range nat0 length_
            |> List.dropFrom LastToFirst 1
            |> Array.fromList
    , length = length_ |> InNat.value
    }
        |> tag
        |> isChecked Arr


random :
    Nat (ArgIn min max ifN_)
    -> Random.Generator element
    -> Random.Generator (Arr (In min max) element)
random amount generateElement =
    Random.list (val amount) generateElement
        |> Random.map
            (\list ->
                { array = Array.fromList list
                , length = amount |> InNat.value
                }
                    |> tag
                    |> isChecked Arr
            )



-- ## modify


replaceAt :
    Nat (ArgIn indexMin minLengthMinus1 indexIfN_)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minLengthMinus1) max) element
    -> Arr (In (Nat1Plus minLengthMinus1) max) element
replaceAt index direction replacingElement =
    mapArray
        (Array.replaceAt (index |> val) direction replacingElement)
        >> isChecked Arr


inPush :
    element
    -> Arr (In min max) element
    -> Arr (In (Nat1Plus min) (Nat1Plus max)) element
inPush element =
    push element (InNat.add nat1)


minPush :
    element
    -> Arr (In min max) element
    -> Arr (Min (Nat1Plus min)) element
minPush element =
    push element (MinNat.add nat1)


{-| **Should not be exposed.**
-}
push :
    element
    -> (Nat (In min max) -> Nat (ArgIn (Nat1Plus min) maxPlus1 ifN_))
    -> Arr (In min max) element
    -> Arr (In (Nat1Plus min) maxPlus1) element
push elementToPush add1 =
    mapArrayAndLength (Array.push elementToPush) add1
        >> isChecked Arr


inInsertAt :
    Nat (ArgIn indexMin minMinus1 indexIfN_)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minMinus1) max) element
    -> Arr (In (Nat2Plus minMinus1) (Nat1Plus max)) element
inInsertAt index direction insertedElement =
    insertAt index
        direction
        insertedElement
        (InNat.add nat1)


minInsertAt :
    Nat (ArgIn indexMin lengthMinus1 indexIfN_)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus lengthMinus1) max) element
    -> Arr (Min (Nat2Plus lengthMinus1)) element
minInsertAt index direction elementToInsert =
    insertAt index
        direction
        elementToInsert
        (MinNat.add nat1)


{-| **Should not be exposed.**
-}
insertAt :
    Nat range
    -> LinearDirection
    -> element
    -> (Nat (In min max) -> Nat (ArgIn (Nat1Plus min) maxPlus1 ifN_))
    -> Arr (In min max) element
    -> Arr (In (Nat1Plus min) maxPlus1) element
insertAt index direction inserted add1 =
    mapArrayAndLength
        (Array.insertAt (index |> val) direction inserted)
        add1
        >> isChecked Arr


inRemoveAt :
    Nat (ArgIn indexMin_ minMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) (Nat1Plus maxMinus1)) element
    -> Arr (In minMinus1 maxMinus1) element
inRemoveAt index direction =
    removeAt index direction (InNat.sub nat1)


minRemoveAt :
    Nat (ArgIn indexMin lengthMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus lengthMinus1) max) element
    -> Arr (In lengthMinus1 max) element
minRemoveAt index direction =
    removeAt index direction (MinNat.sub nat1)


{-| **Should not be exposed.**
-}
removeAt :
    Nat (ArgIn minIndex minMinus1 indexIfN_)
    -> LinearDirection
    ->
        (Nat (In (Nat1Plus minMinus1) max)
         -> Nat (ArgIn minMinus1 maxMinus1 minus1IfN_)
        )
    -> Arr (In (Nat1Plus minMinus1) max) element
    -> Arr (In minMinus1 maxMinus1) element
removeAt index direction sub1 =
    mapArrayAndLength
        (Array.removeAt (val index) direction)
        sub1
        >> isChecked Arr


inExtend :
    Nat (N added atLeastAdded (Is min To sumMin) (Is max To sumMax))
    -> Arr (Only added) element
    -> Arr (In min max) element
    -> Arr (In sumMin sumMax) element
inExtend addedLength extension =
    extend extension (\_ -> InNat.add addedLength)


extendIn :
    Nat (N addedMin atLeastAddedMin_ (Is min To extendedMin) addedMinIs_)
    -> Nat (N addedMax atLeastAddedMax_ (Is max To extendedMax) addedMaxIs_)
    -> Arr (In addedMin addedMax) element
    -> Arr (In min max) element
    -> Arr (In extendedMin extendedMax) element
extendIn extensionMin extensionMax extension =
    extend extension
        (InNat.addIn extensionMin extensionMax)


minExtend :
    Nat (N minAdded atLeastMinAdded_ (Is min To sumMin) is_)
    -> Arr (In minAdded maxAdded) element
    -> Arr (In min max) element
    -> Arr (Min sumMin) element
minExtend minAddedLength extension =
    extend extension (\_ -> MinNat.add minAddedLength)


{-| **Should not be exposed.**
-}
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


inDrop :
    Nat (N dropped_ atLeastDropped_ (Is minTaken To min) (Is maxTaken To max))
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken maxTaken) element
inDrop droppedAmount direction =
    drop droppedAmount direction InNat.sub


minDrop :
    Nat (N dropped_ atLeastDropped_ (Is minTaken To min) is_)
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken max) element
minDrop droppedAmount direction =
    drop droppedAmount direction MinNat.sub


{-| **Should not be exposed.**
-}
drop :
    Nat (N dropped atLeastDropped (Is minTaken To min) is)
    -> LinearDirection
    ->
        (Nat (N dropped atLeastDropped (Is minTaken To min) is)
         -> Nat (In min max)
         -> Nat (In minTaken maxTaken)
        )
    -> Arr (In min max) element
    -> Arr (In minTaken maxTaken) element
drop droppedAmount direction subDropped =
    mapArrayAndLength
        (Array.drop (droppedAmount |> val) direction)
        (\len -> len |> subDropped droppedAmount)
        >> isChecked Arr



-- ## part


takeMax :
    Nat (N maxTaken atLeastMaxTaken (Is maxTakenToMin_ To min) is_)
    -> Nat (ArgIn minTaken maxTaken takenIfN_)
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken atLeastMaxTaken) element
takeMax maxTakenAmount amount direction =
    mapArrayAndLength
        (Array.take (amount |> val) direction)
        (\_ ->
            amount
                |> Nat.restoreMax maxTakenAmount
        )
        >> isChecked Arr


take :
    Nat (N taken atLeastTaken (Is takenToMin_ To min) is_)
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In taken atLeastTaken) element
take takenAmount direction =
    mapArrayAndLength
        (Array.take (takenAmount |> val) direction)
        (\_ -> takenAmount)
        >> isChecked Arr


groupsOf :
    Nat (ArgIn (Nat1Plus minGroupSizMinus1) maxGroupSize groupSizeIfN_)
    -> LinearDirection
    -> Arr (In min max) element
    ->
        { groups :
            Arr
                (In Nat0 max)
                (Arr
                    (In (Nat1Plus minGroupSizMinus1) maxGroupSize)
                    element
                )
        , remaining : Arr (In Nat0 maxGroupSize) element
        }
groupsOf groupSize direction =
    \arr ->
        let
            { groups, less } =
                toArray arr
                    |> Array.group (val groupSize) direction

            remaining =
                less
        in
        { groups =
            { array =
                groups
                    |> Array.map
                        (\array ->
                            { array = array
                            , length = groupSize |> InNat.value
                            }
                                |> tag
                                |> isChecked Arr
                        )
            , length = length arr |> Nat.div groupSize
            }
                |> tag
                |> isChecked Arr
        , remaining =
            { array = remaining
            , length = length arr |> Nat.remainderBy groupSize
            }
                |> tag
                |> isChecked Arr
        }



-- ## drop information


lowerMinLength :
    Nat (ArgIn lowerMin min lowerIfN_)
    -> Arr (In min max) element
    -> Arr (In lowerMin max) element
lowerMinLength validMinimumLength =
    mapLength (Nat.lowerMin validMinimumLength)
        >> isChecked Arr


restoreMaxLength :
    Nat (ArgIn max newMax ifN_)
    -> Arr (In min max) element
    -> Arr (In min newMax) element
restoreMaxLength maximumLength =
    mapLength (Nat.restoreMax maximumLength)
        >> isChecked Arr


minValue : Arr (In min max) element -> Arr (Min min) element
minValue =
    mapLength MinNat.value
        >> isChecked Arr



-- something


resize :
    LinearDirection
    -> Nat (ArgIn newMin newMax ifN_)
    -> element
    -> Arr (In min max) element
    -> Arr (In newMin newMax) element
resize direction newLength defaultElement =
    mapArrayAndLength
        (Array.resize direction (val newLength) defaultElement)
        (\_ -> newLength)
        >> isChecked Arr



-- ## scan length


inIsLength :
    Nat
        (N
            (Nat1Plus valueMinus1)
            atLeastValue
            (Is a_ To (Nat1Plus atLeastValueMinus1))
            (Is valueToMax_ To max)
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is minToValue_ To (Nat1Plus valueMinus1))
                )
        }
    -> Arr (In min max) element
    ->
        Nat.LessOrEqualOrGreater
            (Arr (In lowest atLeastValueMinus1) element)
            (Arr (In (Nat1Plus valueMinus1) atLeastValue) element)
            (Arr (In (Nat2Plus valueMinus1) max) element)
inIsLength amount lowest =
    \arr ->
        let
            withLength len =
                tag { array = toArray arr, length = len }
                    |> isChecked Arr
        in
        case length arr |> InNat.is amount lowest of
            Nat.Less less ->
                Nat.Less (withLength less)

            Nat.Equal equal ->
                Nat.Equal (withLength equal)

            Nat.Greater greater ->
                Nat.Greater (withLength greater)


inIsLengthInRange :
    Nat
        (N
            lowerBound
            (Nat1Plus atLeastLowerBoundMinus1)
            (Is lowerBoundToUpperBound_ To upperBound)
            lowerBoundIs_
        )
    ->
        Nat
            (N
                upperBound
                atLeastUpperBound
                (Is upperBoundToMax_ To max)
                upperBoundIs_
            )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is minToLowerBound_ To lowerBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.BelowOrInOrAboveRange
            (Arr (In lowest atLeastLowerBoundMinus1) element)
            (Arr (In lowerBound atLeastUpperBound) element)
            (Arr (In (Nat1Plus upperBound) max) element)
inIsLengthInRange lowerBound upperBound lowest =
    \arr ->
        let
            withLength len =
                tag { array = toArray arr, length = len }
                    |> isChecked Arr
        in
        case
            length arr
                |> InNat.isInRange lowerBound upperBound lowest
        of
            Nat.BelowRange below ->
                Nat.BelowRange (withLength below)

            Nat.InRange inRange ->
                Nat.InRange (withLength inRange)

            Nat.AboveRange above ->
                Nat.AboveRange (withLength above)


inIsLengthAtLeast :
    Nat
        (N
            lowerBound
            (Nat1Plus atLeastLowerBoundMinus1)
            (Is atLeastRange_ To max)
            is_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest
                    (Is lowestToMin To min)
                    (Is (Nat1Plus lowestToLowerBound_) To lowerBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.BelowOrAtLeast
            (Arr (In lowest atLeastLowerBoundMinus1) element)
            (Arr (In lowerBound max) element)
inIsLengthAtLeast lowerBound lowest =
    \arr ->
        let
            withLength len =
                tag { array = toArray arr, length = len }
                    |> isChecked Arr
        in
        case
            length arr
                |> InNat.isAtLeast lowerBound lowest
        of
            Nat.Below below ->
                Nat.Below (withLength below)

            Nat.EqualOrGreater atLeast ->
                Nat.EqualOrGreater (withLength atLeast)


inIsLengthAtMost :
    Nat
        (N
            upperBound
            atLeastUpperBound
            (Is (Nat1Plus greaterRange_) To max)
            is_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is minToUpperBound_ To upperBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.AtMostOrAbove
            (Arr (In lowest atLeastUpperBound) element)
            (Arr (In (Nat1Plus upperBound) max) element)
inIsLengthAtMost upperBound lowest =
    \arr ->
        let
            withLength len =
                { array = toArray arr, length = len }
                    |> tag
                    |> isChecked Arr
        in
        case
            length arr
                |> InNat.isAtMost upperBound lowest
        of
            Nat.EqualOrLess atMost ->
                Nat.EqualOrLess (withLength atMost)

            Nat.Above above ->
                Nat.Above (withLength above)


minIsLength :
    Nat
        (N
            (Nat1Plus valueMinus1)
            atLeastValue
            (Is a_ To (Nat1Plus atLeastValueMinus1))
            is_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is minToValueMinus1_ To valueMinus1)
                )
        }
    -> Arr (In min max_) element
    ->
        Nat.LessOrEqualOrGreater
            (Arr (In lowest atLeastValueMinus1) element)
            (Arr (In (Nat1Plus valueMinus1) atLeastValue) element)
            (Arr (Min (Nat2Plus valueMinus1)) element)
minIsLength amount lowest =
    \arr ->
        let
            withLength len =
                tag { array = toArray arr, length = len }
                    |> isChecked Arr
        in
        case length arr |> MinNat.is amount lowest of
            Nat.Equal equal ->
                Nat.Equal (withLength equal)

            Nat.Greater greater ->
                Nat.Greater (withLength greater)

            Nat.Less less ->
                Nat.Less (withLength less)


minIsLengthAtLeast :
    Nat
        (ArgIn
            minLowerBound
            (Nat1Plus maxLowerBoundMinus1)
            ifN_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is lowestToMinLowerBound_ To minLowerBound)
                )
        }
    -> Arr (In min max_) element
    ->
        Nat.BelowOrAtLeast
            (Arr (In lowest maxLowerBoundMinus1) element)
            (Arr (Min minLowerBound) element)
minIsLengthAtLeast lowerBound lowest =
    \arr ->
        let
            withLength len =
                tag { array = toArray arr, length = len }
                    |> isChecked Arr
        in
        case length arr |> MinNat.isAtLeast lowerBound lowest of
            Nat.Below less ->
                Nat.Below
                    (withLength less)

            Nat.EqualOrGreater atLeast ->
                Nat.EqualOrGreater (withLength atLeast)


minIsLengthAtMost :
    Nat (ArgIn minUpperBound maxUpperBound ifN_)
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To min)
                    (Is lowestToMinUpperBound_ To minUpperBound)
                )
        }
    -> Arr (In min max_) element
    ->
        Nat.AtMostOrAbove
            (Arr (In lowest maxUpperBound) element)
            (Arr (Min (Nat1Plus minUpperBound)) element)
minIsLengthAtMost upperBound lowest =
    \arr ->
        let
            withLength len =
                tag { array = toArray arr, length = len }
                    |> isChecked Arr
        in
        case length arr |> MinNat.isAtMost upperBound lowest of
            Nat.EqualOrLess atMost ->
                Nat.EqualOrLess (withLength atMost)

            Nat.Above above ->
                Nat.Above (withLength above)



-- ## extra


serialize :
    Nat (ArgIn min max ifN_)
    -> Serialize.Codec String element
    -> Serialize.Codec String (Arr (In min max) element)
serialize length_ serializeElement =
    Serialize.array serializeElement
        |> Serialize.mapValid
            (\array ->
                let
                    decodedLength =
                        Array.length array
                in
                if decodedLength == val length_ then
                    { array = array
                    , length = length_ |> InNat.value
                    }
                        |> tag
                        |> isChecked Arr
                        |> Ok

                else
                    "Array length "
                        ++ String.fromInt decodedLength
                        ++ " was different from the expected length "
                        ++ String.fromInt (val length_)
                        |> Err
            )
            toArray


serializeIn :
    Nat (ArgIn minLowerBound minUpperBound lowerBoundIfN_)
    -> Nat (ArgIn minUpperBound maxUpperBound upperBoundIfN_)
    -> Serialize.Codec String element
    ->
        Serialize.Codec
            String
            (Arr (In minLowerBound maxUpperBound) element)
serializeIn lowerBound upperBound serializeElement =
    Serialize.array serializeElement
        |> Serialize.mapValid
            (\array ->
                let
                    decodedLength =
                        Array.length array

                    err reason bound =
                        "Array length "
                            ++ String.fromInt decodedLength
                            ++ " was "
                            ++ reason
                            ++ " "
                            ++ String.fromInt (val bound)
                            |> Err
                in
                case
                    decodedLength
                        |> Nat.isIntInRange lowerBound upperBound
                of
                    Nat.BelowRange () ->
                        err "less than the expected minimum" lowerBound

                    Nat.AboveRange _ ->
                        err "greater than the expected maximum" upperBound

                    Nat.InRange lengthInRange ->
                        tag { array = array, length = lengthInRange }
                            |> isChecked Arr
                            |> Ok
            )
            toArray


serializeMin :
    Nat (ArgIn min max_ ifN_)
    -> Serialize.Codec String element
    -> Serialize.Codec String (Arr (Min min) element)
serializeMin lowerBound serializeElement =
    Serialize.array serializeElement
        |> Serialize.mapValid
            (\array ->
                let
                    decodedLength =
                        Array.length array
                in
                if decodedLength >= val lowerBound then
                    fromArray array
                        |> mapLength (val >> Nat.intAtLeast lowerBound)
                        |> isChecked Arr
                        |> Ok

                else
                    Err
                        ("Array length "
                            ++ String.fromInt decodedLength
                            ++ " was less than the expected minimum "
                            ++ String.fromInt (val lowerBound)
                        )
            )
            toArray
