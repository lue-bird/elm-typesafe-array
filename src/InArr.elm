module InArr exposing
    ( push, extend, extendIn, removeAt, insertAt, drop
    , isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost
    , serialize
    )

{-| If the maximum length is set to a specific value (also for `Only`),

    -- only up to 50 tags
    tag : Arr (In min_ Nat50) String -> a -> Tagged a

use these operations instead of the ones in `Arr` or `MinArr`.


## modify

@docs push, extend, extendIn, removeAt, insertAt, drop


## scan length

@docs isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost


## extra

@docs serialize

-}

import Arr exposing (Arr)
import Internal
import LinearDirection exposing (LinearDirection(..))
import NNats exposing (..)
import Nat exposing (ArgIn, In, Is, N, Nat, Only, To)
import Serialize
import TypeNats exposing (..)



-- ## modify


{-| Put a new element after the others.

    arrWith5To10Elements
        |> InArr.push "becomes the last"
    --> : Arr (In Nat6 (Nat11Plus a_)) String

-}
push :
    element
    -> Arr (In min max) element
    -> Arr (In (Nat1Plus min) (Nat1Plus max)) element
push element =
    Internal.inPush element


{-| Put an element in the `Arr` at a given index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    Arr.from3 'a' 'c' 'd'
        |> InArr.insertAt nat1 FirstToLast 'b'
    --> Arr.from4 'a' 'b' 'c' 'd'

    Arr.from3 'a' 'c' 'd'
        |> Array.insertAt nat2 LastToFirst 'b'
    --> Array.from4 'a' 'b' 'c' 'd'

-}
insertAt :
    Nat (ArgIn indexMin minMinus1 indexIfN_)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minMinus1) max) element
    -> Arr (In (Nat2Plus minMinus1) (Nat1Plus max)) element
insertAt index direction element =
    Internal.inInsertAt index direction element


{-| Append the elements of an `Arr (In ...)`.

    Arr.from3 1 2 3
        |> InArr.extendIn nat3 nat5
            arrWith3To5Elements
    --> : Arr (In Nat6 (Nat8Plus a))

Use [`extend`](InArr#extend) if the appended `Arr` has an exact amount of elements.

-}
extendIn :
    Nat (N addedMin atLeastAddedMin_ (Is min To extendedMin) addedMinIs_)
    -> Nat (N addedMax atLeastAddedMax_ (Is max To extendedMax) addedMaxIs_)
    -> Arr (In addedMin addedMax) element
    -> Arr (In min max) element
    -> Arr (In extendedMin extendedMax) element
extendIn extensionMin extensionMax extension =
    Internal.extendIn extensionMin extensionMax extension


{-| Append the elements of an `Arr` with an exact amount of elements.

    Arr.from3 1 2 3
        |> InArr.extend nat3 (Arr.from3 4 5 6)
    --> Arr.from6 1 2 3 4 5 6

-}
extend :
    Nat (N added atLeastAdded (Is min To sumMin) (Is max To sumMax))
    -> Arr (Only added) element
    -> Arr (In min max) element
    -> Arr (In sumMin sumMax) element
extend addedLength arrExtension =
    Internal.inExtend addedLength arrExtension


{-| Kick an element out of the `Arr` at a given index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    removeLast arrWith1To10Elements =
        arrWith1To10Elements
            |> InArr.removeAt nat0 LastToFirst

-}
removeAt :
    Nat (ArgIn indexMin minMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) (Nat1Plus maxMinus1)) element
    -> Arr (In minMinus1 maxMinus1) element
removeAt index direction =
    Internal.inRemoveAt index direction


{-| Elements after a certain number of elements in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    Arr.from4 0 1 2 3
        |> InArr.drop nat2 LastToFirst
    --> Arr.from2 0 1

    between6And10Elements
        |> InArr.drop nat2 FirstToLast
    --> : Arr (In Nat4 (Nat10Plus a_)) ...

-}
drop :
    Nat
        (N
            dropped_
            atLeastDropped_
            (Is minTaken To min)
            (Is maxTaken To max)
        )
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken maxTaken) element
drop droppedAmount direction =
    Internal.inDrop droppedAmount direction



-- ## scan length


{-| Compare its length to a given exact length. Is it `LessOrEqualOrGreater`?

`lowest` can be a number <= the minimum length.

    chooseFormation :
        Arr (In min Nat50) Character
        -> Formation
    chooseFormation characters =
        case
            characters
                |> InArr.isLength nat7 { lowest = nat0 }
        of
            Nat.Equal only7 ->
                SpecialAttack only7

            Nat.Less atMost6 ->
                Retreat atMost6

            Nat.Greater atLeast8 ->
                Fight atLeast8

-}
isLength :
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
isLength amount lowest =
    Internal.inIsLength amount lowest


{-| Compared to a range from a lower to an upper bound, is its length `BelowOrInOrAboveRange`?

`lowest` can be a number <= the minimum length.

    chooseFormation :
        Arr (In min Nat50) Character
        -> Formation
    chooseFormation characters =
        case
            characters
                |> InArr.isLengthInRange nat10 nat20 { lowest = nat0 }
        of
            Nat.InRange between10And20 ->
                SpecialAttack between10And20

            Nat.BelowRange atMost9 ->
                Retreat atMost9

            Nat.AboveRange atLeast21 ->
                Fight atLeast21

-}
isLengthInRange :
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
isLengthInRange lowerBound upperBound lowest =
    Internal.inIsLengthInRange lowerBound upperBound lowest


{-| Is its length `BelowOrAtLeast` as big as a `Nat`?

`lowest` can be a number <= the minimum length.

    first5 :
        Arr (In min max) element
        -> Maybe (Arr (In Nat5 max) element)
    first5 arr =
        case
            arr |> MinArr.isLengthAtLeast nat5 { lowest = nat0 }
        of
            Nat.Below _ ->
                Nothing

            Nat.EqualOrGreater atLeast5 ->
                Just atLeast5

-}
isLengthAtLeast :
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
isLengthAtLeast lowerBound lowest =
    Internal.inIsLengthAtLeast lowerBound lowest


{-| Is its length `AtMostOrAbove` a given length?

`lowest` can be a number <= the minimum length.

    -- at least 3 and only up to 50 tags
    tag :
        Arr (In (Nat3Plus orHigherMin) Nat50) String
        -> a
        -> Tagged a

    tagIfValidTags :
        Arr (In (Nat3Plus orHigherMin) max)
        -> a
        -> Maybe (Tagged a)
    tagIfValidTags array value =
        case
            array
                |> Arr.fromArray
                |> InArr.isLengthAtMost nat50 { lowest = nat0 }
        of
            Nat.EqualOrLess atMost53 ->
                tag value atMost53 |> Just

            Nat.Above _ ->
                Nothing

-}
isLengthAtMost :
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
isLengthAtMost upperBound lowest =
    Internal.inIsLengthAtMost upperBound lowest


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize `Arr`s within a minimum & maximum length.

    import Serialize

    serialize10To15Ints :
        Serialize.Codec
            String
            (Arr (In Nat10 (Nat15Plus a_)) Int)
    serialize10To15Ints =
        InArr.serialize Serialize.int nat10 nat15

    encode :
        Arr (In (Nat10Plus orHigherMin_) Nat15) Int
        -> Bytes
    encode =
        Arr.lowerMinLength nat10
            >> Serialize.encodeToBytes serialize10To15Ints

    decode :
        Bytes
        ->
            Result
                (Serialize.Error String)
                (Arr (In Nat10 (Nat15Plus a_) Int))
    decode =
        Serialize.decodeFromBytes serialize10To15Ints

For decoded `Arr`s with a length outside of the expected bounds, the `Result` is an error message.

-}
serialize :
    Nat (ArgIn minLowerBound minUpperBound lowerBoundIfN_)
    -> Nat (ArgIn minUpperBound maxUpperBound upperBoundIfN_)
    -> Serialize.Codec String element
    ->
        Serialize.Codec
            String
            (Arr (In minLowerBound maxUpperBound) element)
serialize lowerBound upperBound serializeElement =
    Internal.serializeIn lowerBound upperBound serializeElement
