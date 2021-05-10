module InArr exposing
    ( push, extend, extendIn, removeAt, insertAt, drop
    , isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost
    , serialize
    )

{-| If the maximum length is set to a specific value (also for `Only`),

    -- only up to 50 tags
    tag : Arr (In min Nat50) String -> a -> Tagged a

use these operations instead of the ones in `Arr` or `MinArr`.


## modify

@docs push, extend, extendIn, removeAt, insertAt, drop


## scan length

@docs isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost


## extra

@docs serialize

-}

import Arr exposing (Arr)
import Internal.InArr as Internal
import LinearDirection exposing (LinearDirection(..))
import NNats exposing (..)
import Nat exposing (ArgIn, ArgN, In, Is, N, Nat, Only, To)
import Serialize
import TypeNats exposing (..)



-- ## modify


{-| Put a new element after the others.

    arrWith5To10Elements
        |> InArr.push "becomes the last"
    --> : Arr (In Nat6 Nat11) String

-}
push :
    element
    -> Arr (In min max) element
    -> Arr (In (Nat1Plus min) (Nat1Plus max)) element
push element =
    Internal.push element


{-| Put an element in the `Arr` at a given index in a direction.

    Arr.from3 'a' 'c' 'd'
        |> InArr.insertAt nat1 FirstToLast 'b'
    --> Arr.from4 'a' 'b' 'c' 'd'

    Arr.from3 'a' 'c' 'd'
        |> Array.insertAt nat2 LastToFirst 'b'
    --> Array.from4 'a' 'b' 'c' 'd'

-}
insertAt :
    Nat (ArgIn indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minMinus1) max) element
    -> Arr (In (Nat2Plus minMinus1) (Nat1Plus max)) element
insertAt index direction element =
    Internal.insertAt index direction element


{-| Append the elements of another `Arr (In ...)`.

    Arr.from3 1 2 3
        |> extendIn nat3 nat5 arrWith3To5Elements
    --> : Arr (In Nat6 (Nat8Plus a))

-}
extendIn :
    Nat (ArgN extensionMin (Is min To extendedMin) x)
    -> Nat (ArgN extensionMax (Is max To extendedMax) y)
    -> Arr (In extensionMin extensionMax) element
    -> Arr (In min max) element
    -> Arr (In extendedMin extendedMax) element
extendIn extensionMin extensionMax extension =
    Internal.extendIn extensionMin extensionMax extension


{-| Append the elements of an `Arr (Only ...)`.

    Arr.from3 1 2 3
        |> extend nat3 (Arr.from3 4 5 6)
    --> Arr.from6 1 2 3 4 5 6

-}
extend :
    Nat (ArgN added (Is min To sumMin) (Is max To sumMax))
    -> Arr (Only added) element
    -> Arr (In min max) element
    -> Arr (In sumMin sumMax) element
extend addedLength arrExtension =
    Internal.extend addedLength arrExtension


{-| Kick an element out of the `Arr` at a given index in a direction.

    removeLast arrWith1To10Elements =
        arrWith1To10Elements
            |> InArr.removeAt nat0 LastToFirst

-}
removeAt :
    Nat (ArgIn indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) (Nat1Plus maxMinus1)) element
    -> Arr (In minMinus1 maxMinus1) element
removeAt index direction =
    Internal.removeAt index direction


{-| Elements after a certain number of elements from one side.

    with6To10Elements
        |> Arr.drop nat2 LastToFirst
    --> : Arr (In Nat4 (Nat10Plus a)) ...

-}
drop :
    Nat (ArgN dropped (Is minTaken To min) (Is maxTaken To max))
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken maxTaken) element
drop droppedAmount direction =
    Internal.drop droppedAmount direction



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
        (ArgN
            value
            (Is valueToMax To max)
            (Is a To (Nat1Plus atLeastValueMinus1))
        )
    ->
        { lowest :
            Nat
                (ArgN
                    lowest
                    (Is lowestToMin To min)
                    (Is minToValue To value)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.LessOrEqualOrGreater
            (Arr (In lowest atLeastValueMinus1) element)
            (Arr (Only value) element)
            (Arr (In (Nat2Plus valueMinus1) max) element)
isLength amount lowest =
    Internal.isLength amount lowest


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
        (ArgN
            lowerBound
            (Is lowerBoundToLast To upperBound)
            (Is lowerBoundA To (Nat1Plus atLeastFirstMinus1))
        )
    -> Nat (ArgN upperBound (Is upperBoundToMax To max) (Is upperBoundA To atLeastLast))
    ->
        { lowest :
            Nat
                (ArgN
                    lowest
                    (Is lowestToMin To min)
                    (Is minToFirst To lowerBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.BelowOrInOrAboveRange
            (Arr (In lowest atLeastFirstMinus1) element)
            (Arr (In lowerBound atLeastLast) element)
            (Arr (In (Nat1Plus upperBound) max) element)
isLengthInRange lowerBound upperBound lowest =
    Internal.isLengthInRange lowerBound upperBound lowest


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
        (ArgN
            lowerBound
            (Is a To (Nat1Plus atLeastLowerBoundMinus1))
            (Is atLeastRange To max)
        )
    ->
        { lowest :
            Nat
                (ArgN
                    lowest
                    (Is lowestToMin To min)
                    (Is (Nat1Plus lessRange) To lowerBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.BelowOrAtLeast
            (Arr (In lowest atLeastLowerBoundMinus1) element)
            (Arr (In lowerBound max) element)
isLengthAtLeast lowerBound lowest =
    Internal.isLengthAtLeast lowerBound lowest


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
        (ArgN
            upperBound
            (Is a To atLeastUpperBound)
            (Is (Nat1Plus greaterRange) To max)
        )
    ->
        { lowest :
            Nat
                (ArgN
                    lowest
                    (Is lowestToMin To min)
                    (Is minToUpperBound To upperBound)
                )
        }
    -> Arr (In min max) element
    ->
        Nat.AtMostOrAbove
            (Arr (In lowest atLeastUpperBound) element)
            (Arr (In (Nat1Plus upperBound) max) element)
isLengthAtMost upperBound lowest =
    Internal.isLengthAtMost upperBound lowest


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize `Arr`s within a minimum & maximum length.

    import Serialize

    serialize10To15Ints :
        Serialize.Codec
            String
            (Arr (In Nat10 (Nat15Plus a)) Int)
    serialize10To15Ints =
        InArr.serialize Serialize.int nat10 nat15

    encode :
        Arr (In (Nat10Plus orHigherMin) Nat15) Int
        -> Bytes
    encode =
        Arr.lowerMinLength nat10
            >> Serialize.encodeToBytes serialize10To15Ints

    decode :
        Bytes
        ->
            Result
                (Serialize.Error String)
                (Arr (In Nat10 (Nat15Plus a) Int))
    decode =
        Serialize.decodeFromBytes serialize10To15Ints

For decoded `Arr`s with a length outside of the expected bounds, the `Result` is an error message.

-}
serialize :
    Nat (ArgIn minLowerBound minUpperBound lowerBoundMaybeN)
    -> Nat (ArgIn minUpperBound maxUpperBoundPlusA upperBoundMaybeN)
    -> Serialize.Codec String element
    ->
        Serialize.Codec
            String
            (Arr (In minLowerBound maxUpperBoundPlusA) element)
serialize lowerBound upperBound serializeElement =
    Internal.serialize lowerBound upperBound serializeElement
