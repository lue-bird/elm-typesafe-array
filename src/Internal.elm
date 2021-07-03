module Internal exposing
    ( empty, fromArray, repeat, nats, minNats, random
    , at, length
    , inIsLengthInRange, inIsLength, inIsLengthAtLeast, inIsLengthAtMost, minIsLength, minIsLengthAtLeast, minIsLengthAtMost
    , toArray, map, map2
    , serialize, serializeIn, serializeMin, SerializeInRangeError(..), serializeErrorToString
    , replaceAt, inPush, minPush, inInsertAt, minInsertAt, inRemoveAt, minRemoveAt, resize, order
    , appendIn, inAppend, minAppend, minPrepend, inPrepend, prependIn
    , when, whenJust
    , take, takeMax, inDrop, minDrop, groupsOf
    , ArrTag, Content, lowerMinLength, minValue, restoreMaxLength
    )

{-| Contains stuff that is unsafe to use.

Calling `isChecked Arr` marks unsafe operations.


## create

@docs empty, fromArray, repeat, nats, minNats, random


## scan

@docs at, length


### scan length

@docs inIsLengthInRange, inIsLength, inIsLengthAtLeast, inIsLengthAtMost, minIsLength, minIsLengthAtLeast, minIsLengthAtMost


## transform

@docs toArray, map, map2


### serialize

@docs serialize, serializeIn, serializeMin, SerializeInRangeError, serializeErrorToString


## modify

@docs replaceAt, inPush, minPush, inInsertAt, minInsertAt, inRemoveAt, minRemoveAt, resize, order


### glue

@docs appendIn, inAppend, minAppend, minPrepend, inPrepend, prependIn


### filter

@docs when, dropWhen, whenJust


### part

@docs take, takeMax, inDrop, minDrop, groupsOf

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
import Serialize exposing (Codec)
import TypeNats exposing (..)
import Typed exposing (Checked, Internal, Tagged, Typed, internalVal, internalVal2, isChecked, tag, val)


type alias Arr length element =
    ArrAs Checked length element


type alias ArrAs whoCanCreate length element =
    Typed whoCanCreate ArrTag Internal (Content length element)


type alias Content length element =
    { array : Array element, length : Nat length }


{-| **Constructor should not be exposed!**
-}
type ArrTag
    = Arr


from :
    Array element
    -> { length : Nat (ArgIn minLength maxLength ifN_) }
    -> ArrAs Tagged (In minLength maxLength) element
from array length_ =
    { array = array
    , length = length_.length |> InNat.value
    }
        |> tag



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
                -- instead of failing silently
                -- like Orasund's static-array does
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
    -> (Nat length -> Nat (ArgIn mappedMin mappedMax mappedIfN_))
    -> ArrAs whoCreated_ length element
    -> ArrAs Tagged (In mappedMin mappedMax) mappedElement
mapArrayAndLength mapArray_ mapLength_ =
    Typed.map
        (\v ->
            { array = mapArray_ v.array
            , length = mapLength_ v.length |> InNat.value
            }
        )


mapLength :
    (Nat length -> Nat (ArgIn mappedMin mappedMax mappedIfN_))
    -> ArrAs whoCreated_ length element
    -> ArrAs Tagged (In mappedMin mappedMax) element
mapLength mapLength_ =
    mapArrayAndLength identity mapLength_


mapArray :
    (Array element -> Array mappedElement)
    -> ArrAs whoCreated_ length element
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


order :
    LinearDirection
    -> Arr length element
    -> Arr length element
order direction =
    mapArray (Array.order direction)
        >> isChecked Arr


when :
    (element -> Bool)
    -> Arr (In min max) element
    -> Arr (In Nat0 max) element
when isGood =
    mapArrayAndLength
        (Array.filter isGood)
        (Nat.lowerMin nat0)
        >> isChecked Arr


whenJust :
    Arr (In min_ max) (Maybe value)
    -> Arr (In Nat0 max) value
whenJust maybes =
    maybes
        |> toArray
        |> Array.Extra.filterMap identity
        |> fromArray
        |> mapLength
            (Nat.atMost (length maybes)
                { lowest = nat0 }
            )
        |> isChecked Arr



-- ## create


empty : Arr (In Nat0 atLeast0) element
empty =
    from Array.empty { length = nat0 }
        |> isChecked Arr


repeat :
    Nat (ArgIn min max ifN_)
    -> element
    -> Arr (In min max) element
repeat amount element =
    from (Array.repeat (val amount) element)
        { length = amount }
        |> isChecked Arr


fromArray : Array element -> Arr (Min Nat0) element
fromArray array =
    from array
        { length =
            Array.length array
                |> Nat.intAtLeast nat0
        }
        |> isChecked Arr


nats :
    Nat (ArgIn minLength (Nat1Plus maxLengthMinus1) lengthIfN_)
    ->
        Arr
            (In minLength (Nat1Plus maxLengthMinus1))
            (Nat (In Nat0 maxLengthMinus1))
nats length_ =
    from
        (case length_ |> InNat.isAtLeast nat1 { lowest = nat0 } of
            Nat.EqualOrGreater lengthAtLeast1 ->
                List.range 0 (val length_ - 1)
                    |> List.map
                        (Nat.intInRange nat0
                            (lengthAtLeast1 |> InNat.sub nat1)
                        )
                    |> Array.fromList

            Nat.Below _ ->
                Array.empty
        )
        { length = length_ }
        |> isChecked Arr


minNats :
    Nat (ArgIn minLength maxLength ifN_)
    ->
        Arr
            (In minLength maxLength)
            (Nat (In Nat0 maxLength))
minNats length_ =
    from
        (Nat.range nat0 length_
            |> List.dropFrom LastToFirst 1
            |> Array.fromList
        )
        { length = length_ }
        |> isChecked Arr


random :
    Nat (ArgIn min max ifN_)
    -> Random.Generator element
    -> Random.Generator (Arr (In min max) element)
random amount generateElement =
    Random.list (val amount) generateElement
        |> Random.map
            (\list ->
                from (Array.fromList list) { length = amount }
                    |> isChecked Arr
            )



-- ## modify


replaceAt :
    Nat index_
    -> LinearDirection
    -> element
    -> Arr length element
    -> Arr length element
replaceAt index direction replacement =
    mapArray
        (Array.replaceAt (val index) direction replacement)
        >> isChecked Arr


inPush :
    element
    -> Arr (In min max) element
    -> Arr (In (Nat1Plus min) (Nat1Plus max)) element
inPush element =
    push element (InNat.add nat1)
        >> isChecked Arr


minPush :
    element
    -> Arr (In min max) element
    -> Arr (Min (Nat1Plus min)) element
minPush element =
    push element (MinNat.add nat1)
        >> isChecked Arr


{-| Should not be exposed.
-}
push :
    element
    -> (Nat (In min max) -> Nat (ArgIn (Nat1Plus min) maxPlus1 ifN_))
    -> Arr (In min max) element
    -> ArrAs Tagged (In (Nat1Plus min) maxPlus1) element
push elementToPush add1 =
    mapArrayAndLength (Array.push elementToPush) add1


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
        >> isChecked Arr


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
        >> isChecked Arr


{-| Should not be exposed.
-}
insertAt :
    Nat range
    -> LinearDirection
    -> element
    -> (Nat (In min max) -> Nat (ArgIn (Nat1Plus min) maxPlus1 ifN_))
    -> Arr (In min max) element
    -> ArrAs Tagged (In (Nat1Plus min) maxPlus1) element
insertAt index direction inserted add1 =
    mapArrayAndLength
        (Array.insertAt (val index) direction inserted)
        add1


inRemoveAt :
    Nat (ArgIn indexMin_ minMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) (Nat1Plus maxMinus1)) element
    -> Arr (In minMinus1 maxMinus1) element
inRemoveAt index direction =
    removeAt index direction (InNat.sub nat1)
        >> isChecked Arr


minRemoveAt :
    Nat (ArgIn indexMin lengthMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus lengthMinus1) max) element
    -> Arr (In lengthMinus1 max) element
minRemoveAt index direction =
    removeAt index direction (MinNat.sub nat1)
        >> isChecked Arr


{-| Should not be exposed.
-}
removeAt :
    Nat (ArgIn minIndex minMinus1 indexIfN_)
    -> LinearDirection
    ->
        (Nat (In (Nat1Plus minMinus1) max)
         -> Nat (ArgIn minMinus1 maxMinus1 minus1IfN_)
        )
    -> Arr (In (Nat1Plus minMinus1) max) element
    -> ArrAs Tagged (In minMinus1 maxMinus1) element
removeAt index direction sub1 =
    mapArrayAndLength
        (Array.removeAt (val index) direction)
        sub1


inAppend :
    Nat (N added atLeastAdded (Is min To sumMin) (Is max To sumMax))
    -> Arr (Only added) element
    -> Arr (In min max) element
    -> Arr (In sumMin sumMax) element
inAppend addedLength extension =
    append extension (\_ -> InNat.add addedLength)
        >> isChecked Arr


appendIn :
    Nat (N addedMin atLeastAddedMin_ (Is min To appendedMin) addedMinIs_)
    -> Nat (N addedMax atLeastAddedMax_ (Is max To appendedMax) addedMaxIs_)
    -> Arr (In addedMin addedMax) element
    -> Arr (In min max) element
    -> Arr (In appendedMin appendedMax) element
appendIn extensionMin extensionMax extension =
    append extension
        (InNat.addIn extensionMin extensionMax)
        >> isChecked Arr


minAppend :
    Nat (N minAdded atLeastMinAdded_ (Is min To sumMin) is_)
    -> Arr (In minAdded maxAdded) element
    -> Arr (In min max) element
    -> Arr (Min sumMin) element
minAppend minAddedLength extension =
    append extension (\_ -> MinNat.add minAddedLength)
        >> isChecked Arr


{-| Should not be exposed.
-}
glue :
    ((Array element -> Array element -> Array element)
     -> Array element
     -> Array element
     -> Array element
    )
    -> Arr addedLength element
    -> (Nat addedLength -> Nat length -> Nat lengthSum)
    -> Arr length element
    -> ArrAs Tagged lengthSum element
glue direction extension addLength =
    let
        appendVal a b =
            { array = direction Array.append b.array a.array
            , length = addLength a.length b.length
            }
    in
    internalVal2 appendVal Arr extension Arr
        >> tag


{-| Should not be exposed.
-}
append :
    Arr addedLength element
    -> (Nat addedLength -> Nat length -> Nat lengthSum)
    -> Arr length element
    -> ArrAs Tagged lengthSum element
append extension addLength =
    glue (\app a b -> app a b) extension addLength


inPrepend :
    Nat (N added atLeastAdded (Is min To sumMin) (Is max To sumMax))
    -> Arr (Only added) element
    -> Arr (In min max) element
    -> Arr (In sumMin sumMax) element
inPrepend addedLength extension =
    prepend extension (\_ -> InNat.add addedLength)
        >> isChecked Arr


{-| Should not be exposed.
-}
prepend :
    Arr addedLength element
    -> (Nat addedLength -> Nat length -> Nat lengthSum)
    -> Arr length element
    -> ArrAs Tagged lengthSum element
prepend extension addLength =
    glue (\app a b -> app b a) extension addLength


prependIn :
    Nat (N addedMin atLeastAddedMin_ (Is min To appendedMin) addedMinIs_)
    -> Nat (N addedMax atLeastAddedMax_ (Is max To appendedMax) addedMaxIs_)
    -> Arr (In addedMin addedMax) element
    -> Arr (In min max) element
    -> Arr (In appendedMin appendedMax) element
prependIn extensionMin extensionMax extension =
    prepend extension
        (InNat.addIn extensionMin extensionMax)
        >> isChecked Arr


minPrepend :
    Nat (N minAdded atLeastMinAdded_ (Is min To sumMin) is_)
    -> Arr (In minAdded maxAdded) element
    -> Arr (In min max) element
    -> Arr (Min sumMin) element
minPrepend minAddedLength extension =
    prepend extension (\_ -> MinNat.add minAddedLength)
        >> isChecked Arr


inDrop :
    Nat (N dropped_ atLeastDropped_ (Is minTaken To min) (Is maxTaken To max))
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken maxTaken) element
inDrop droppedAmount direction =
    drop droppedAmount direction InNat.sub
        >> isChecked Arr


minDrop :
    Nat (N dropped_ atLeastDropped_ (Is minTaken To min) is_)
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken max) element
minDrop droppedAmount direction =
    drop droppedAmount direction MinNat.sub
        >> isChecked Arr


{-| Should not be exposed.
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
    -> ArrAs Tagged (In minTaken maxTaken) element
drop droppedAmount direction subDropped =
    mapArrayAndLength
        (Array.drop (val droppedAmount) direction)
        (\len -> len |> subDropped droppedAmount)



-- ## part


{-| Should not be exposed.
-}
takeTemplate :
    Nat (ArgIn minTaken maxTaken mappedIfN_)
    -> LinearDirection
    -> Arr length element
    -> ArrAs Tagged (In minTaken maxTaken) element
takeTemplate amountToTake direction =
    mapArrayAndLength
        (Array.take (val amountToTake) direction)
        (\_ -> amountToTake)


takeMax :
    Nat (N maxTaken atLeastMaxTaken (Is maxTakenToMin_ To min) is_)
    -> Nat (ArgIn minTaken maxTaken takenIfN_)
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken atLeastMaxTaken) element
takeMax maxTakenAmount amountToTake direction =
    takeTemplate
        (amountToTake |> Nat.restoreMax maxTakenAmount)
        direction
        >> isChecked Arr


take :
    Nat (N taken atLeastTaken (Is takenToMin_ To min) is_)
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In taken atLeastTaken) element
take amountToTake direction =
    takeTemplate amountToTake direction
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
        -- find a safer (less isChecked Arr) way
        let
            { groups, less } =
                toArray arr
                    |> Array.group (val groupSize) direction
        in
        { groups =
            from
                (groups
                    |> Array.map
                        (\array ->
                            from array { length = groupSize }
                                |> isChecked Arr
                        )
                )
                { length = length arr |> Nat.div groupSize }
                |> isChecked Arr
        , remaining =
            from less
                { length =
                    length arr
                        |> Nat.remainderBy groupSize
                }
                |> isChecked Arr
        }



-- ## drop information


lowerMinLength :
    Nat (ArgIn newMin min lowerIfN_)
    -> Arr (In min max) element
    -> Arr (In newMin max) element
lowerMinLength newMinimumLength =
    mapLength (Nat.lowerMin newMinimumLength)
        >> isChecked Arr


restoreMaxLength :
    Nat (ArgIn max newMax ifN_)
    -> Arr (In min max) element
    -> Arr (In min newMax) element
restoreMaxLength newMaximumLength =
    mapLength (Nat.restoreMax newMaximumLength)
        >> isChecked Arr


minValue : Arr (In min max) element -> Arr (Min min) element
minValue =
    mapLength MinNat.value >> isChecked Arr


resize :
    LinearDirection
    -> Nat (ArgIn newMin newMax ifN_)
    -> element
    -> Arr length_ element
    -> Arr (In newMin newMax) element
resize direction newLength paddingValue =
    mapArrayAndLength
        (Array.resize direction
            (val newLength)
            paddingValue
        )
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
                from (toArray arr) { length = len }
        in
        case length arr |> InNat.is amount lowest of
            Nat.Less less ->
                Nat.Less (withLength less |> isChecked Arr)

            Nat.Equal equal ->
                Nat.Equal (withLength equal |> isChecked Arr)

            Nat.Greater greater ->
                Nat.Greater (withLength greater |> isChecked Arr)


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
                from (toArray arr) { length = len }
        in
        case
            length arr
                |> InNat.isInRange lowerBound upperBound lowest
        of
            Nat.BelowRange below ->
                Nat.BelowRange
                    (withLength below |> isChecked Arr)

            Nat.InRange inRange ->
                Nat.InRange
                    (withLength inRange |> isChecked Arr)

            Nat.AboveRange above ->
                Nat.AboveRange
                    (withLength above |> isChecked Arr)


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
                from (toArray arr) { length = len }
        in
        case
            length arr
                |> InNat.isAtLeast lowerBound lowest
        of
            Nat.Below below ->
                Nat.Below
                    (withLength below |> isChecked Arr)

            Nat.EqualOrGreater atLeast ->
                Nat.EqualOrGreater
                    (withLength atLeast |> isChecked Arr)


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
                from (toArray arr) { length = len }
        in
        case
            length arr
                |> InNat.isAtMost upperBound lowest
        of
            Nat.EqualOrLess atMost ->
                Nat.EqualOrLess
                    (withLength atMost |> isChecked Arr)

            Nat.Above above ->
                Nat.Above
                    (withLength above |> isChecked Arr)


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
                from (toArray arr) { length = len }
        in
        case length arr |> MinNat.is amount lowest of
            Nat.Equal equal ->
                Nat.Equal
                    (withLength equal |> isChecked Arr)

            Nat.Greater greater ->
                Nat.Greater
                    (withLength greater |> isChecked Arr)

            Nat.Less less ->
                Nat.Less
                    (withLength less |> isChecked Arr)


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
                from (toArray arr) { length = len }
        in
        case length arr |> MinNat.isAtLeast lowerBound lowest of
            Nat.Below less ->
                Nat.Below
                    (withLength less |> isChecked Arr)

            Nat.EqualOrGreater atLeast ->
                Nat.EqualOrGreater
                    (withLength atLeast |> isChecked Arr)


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
                from (toArray arr) { length = len }
        in
        case length arr |> MinNat.isAtMost upperBound lowest of
            Nat.EqualOrLess atMost ->
                Nat.EqualOrLess
                    (withLength atMost |> isChecked Arr)

            Nat.Above above ->
                Nat.Above
                    (withLength above |> isChecked Arr)



-- ## serialize


{-| Should not be exposed.
-}
serializeValid :
    (Array element
     -> Result expectedLength (Arr length element)
    )
    -> Codec serializeError element
    ->
        ({ expectedLength : expectedLength
         , actualLength : Int
         }
         -> serializeError
        )
    -> Codec serializeError (Arr length element)
serializeValid mapValid serializeElement toSerializeError =
    Serialize.array
        serializeElement
        |> Serialize.mapValid
            (\array ->
                mapValid array
                    |> Result.mapError
                        (\expectedLength ->
                            { expectedLength = expectedLength
                            , actualLength = Array.length array
                            }
                                |> toSerializeError
                        )
            )
            toArray


serialize :
    Nat (ArgIn min max ifN)
    ->
        ({ expectedLength : Nat (ArgIn min max ifN)
         , actualLength : Int
         }
         -> serializeError
        )
    -> Codec serializeError element
    -> Codec serializeError (Arr (In min max) element)
serialize length_ toSerializeError serializeElement =
    serializeValid
        (\array ->
            if Array.length array == val length_ then
                from array { length = length_ }
                    |> isChecked Arr
                    |> Ok

            else
                Err length_
        )
        serializeElement
        toSerializeError


type SerializeInRangeError minimum maximum
    = AtLeast (Nat minimum)
    | AtMost (Nat maximum)


serializeIn :
    Nat (ArgIn minLowerBound minUpperBound lowerBoundIfN)
    -> Nat (ArgIn minUpperBound maxUpperBound upperBoundIfN)
    ->
        ({ expectedLength :
            SerializeInRangeError
                (ArgIn minLowerBound minUpperBound lowerBoundIfN)
                (ArgIn minUpperBound maxUpperBound upperBoundIfN)
         , actualLength : Int
         }
         -> serializeError
        )
    -> Codec serializeError element
    ->
        Codec
            serializeError
            (Arr (In minLowerBound maxUpperBound) element)
serializeIn lowerBound upperBound toSerializeError serializeElement =
    serializeValid
        (\array ->
            case
                Array.length array
                    |> Nat.isIntInRange lowerBound upperBound
            of
                Nat.InRange lengthInRange ->
                    from array { length = lengthInRange }
                        |> isChecked Arr
                        |> Ok

                Nat.BelowRange () ->
                    Err (AtLeast lowerBound)

                Nat.AboveRange _ ->
                    Err (AtMost upperBound)
        )
        serializeElement
        toSerializeError


serializeMin :
    Nat (ArgIn min max ifN)
    ->
        ({ expectedLength : { atLeast : Nat (ArgIn min max ifN) }
         , actualLength : Int
         }
         -> serializeError
        )
    -> Codec serializeError element
    -> Codec serializeError (Arr (Min min) element)
serializeMin lowerBound toSerializeError serializeElement =
    serializeValid
        (\array ->
            case
                Array.length array
                    |> Nat.isIntAtLeast lowerBound
            of
                Just validLength ->
                    from array { length = validLength }
                        |> isChecked Arr
                        |> Ok

                Nothing ->
                    Err { atLeast = lowerBound }
        )
        serializeElement
        toSerializeError


serializeErrorToString :
    (expectedLength -> String)
    ->
        { expectedLength : expectedLength
        , actualLength : Int
        }
    -> String
serializeErrorToString expectedLengthToString error =
    [ "expected an array of length"
    , expectedLengthToString error.expectedLength
    , "but the actual length was"
    , String.fromInt error.actualLength
    ]
        |> String.join " "
