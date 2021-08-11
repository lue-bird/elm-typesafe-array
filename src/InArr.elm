module InArr exposing
    ( push, removeAt, insertAt
    , append, appendIn, prepend, prependIn
    , drop
    , isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost
    , serialize, serializeIn
    , Error, generalizeError, errorToString
    )

{-| If the maximum length is set to a specific value (also for `Only`),

    -- only up to 50 tags
    tag : Arr (In min_ Nat50) String -> a -> Tagged a

use these operations instead of the ones in `Arr` or `MinArr`.


# modify

@docs push, removeAt, insertAt


## glue

@docs append, appendIn, prepend, prependIn


## part

@docs drop


# scan length

@docs isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost


# transform


## serialize

@docs serialize, serializeIn


## error

@docs Error, generalizeError, errorToString

-}

import Arr exposing (Arr)
import Common exposing (fromInternalError)
import Internal
import LinearDirection exposing (LinearDirection(..))
import Nat exposing (ArgIn, In, Is, Min, N, Nat, Only, To)
import Nats exposing (..)
import Serialize exposing (Codec)



-- ## modify


{-| Equivalent to `insertAt nat0 LastToFirst`. Put a new element after all the others.

    between5And10Elements
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
    Nat (ArgIn indexMin_ minLength indexIfN_)
    -> LinearDirection
    -> element
    -> Arr (In minLength maxLength) element
    -> Arr (In (Nat1Plus minLength) (Nat1Plus maxLength)) element
insertAt index direction element =
    Internal.inInsertAt index direction element


{-| Attach elements of an `Arr` which has multiple possible amounts to the right.

    Arr.from3 1 2 3
        |> InArr.appendIn nat3 nat5
            between3And5Elements
    --> : Arr (In Nat6 (Nat8Plus a))

Use [`append`](InArr#append) if the appended `Arr` has an exact amount of elements.

-}
appendIn :
    Nat (N addedMin atLeastAddedMin_ (Is min To appendedMin) addedMinIs_)
    -> Nat (N addedMax atLeastAddedMax_ (Is max To appendedMax) addedMaxIs_)
    -> Arr (In addedMin addedMax) element
    -> Arr (In min max) element
    -> Arr (In appendedMin appendedMax) element
appendIn extensionMin extensionMax extension =
    Internal.appendIn extensionMin extensionMax extension


{-| Add elements of an `Arr` which has multiple possible amounts to the left.

    Arr.from3 1 2 3
        |> InArr.prependIn nat3 nat5
            between3And5Elements
    --> : Arr (In Nat6 (Nat8Plus a))

Use [`prepend`](InArr#prepend) if the appended `Arr` has an exact amount of elements.

-}
prependIn :
    Nat (N addedMin atLeastAddedMin_ (Is min To appendedMin) addedMinIs_)
    -> Nat (N addedMax atLeastAddedMax_ (Is max To appendedMax) addedMaxIs_)
    -> Arr (In addedMin addedMax) element
    -> Arr (In min max) element
    -> Arr (In appendedMin appendedMax) element
prependIn extensionMin extensionMax extension =
    Internal.prependIn extensionMin extensionMax extension


{-| Attach elements of an `Arr` with an exact amount of elements to the right.

    Arr.from3 1 2 3
        |> InArr.append nat3 (Arr.from3 4 5 6)
    --> Arr.from6 1 2 3 4 5 6

-}
append :
    Nat (N added atLeastAdded_ (Is min To sumMin) (Is max To sumMax))
    -> Arr (Only added) element
    -> Arr (In min max) element
    -> Arr (In sumMin sumMax) element
append addedLength arrExtension =
    Internal.inAppend addedLength arrExtension


{-| Add elements of an `Arr` with an exact amount of elements to the left.

    Arr.from3 1 2 3
        |> InArr.prepend nat3 (Arr.from3 4 5 6)
    --> Arr.from6 4 5 6 1 2 3

-}
prepend :
    Nat (N added atLeastAdded_ (Is min To sumMin) (Is max To sumMax))
    -> Arr (Only added) element
    -> Arr (In min max) element
    -> Arr (In sumMin sumMax) element
prepend addedLength arrExtension =
    Internal.inPrepend addedLength arrExtension


{-| Kick an element out of the `Arr` at a given index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    removeLast between1And10Elements =
        between1And10Elements
            |> InArr.removeAt nat0 LastToFirst

-}
removeAt :
    Nat (ArgIn indexMin_ minMinus1 indexIfN_)
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
                    atLeastLowest_
                    (Is lowestToMin_ To min)
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
        Arr (In (Nat3Plus orHigherMin_) Nat50) String
        -> a
        -> Tagged a

    tagIfValidTags :
        Arr (In (Nat3Plus orHigherMin_) max)
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

    import Serialize exposing (Codec)

    serialize10To15Ints :
        Codec
            String
            (Arr (In Nat10 (Nat15Plus a_)) Int)
    serialize10To15Ints =
        InArr.serializeIn
            nat10
            nat15
            -- if we just want a simple error string
            InArr.errorToString
            Serialize.int

The encode/decode functions can be extracted if needed.

    encode :
        Arr (In (Nat10Plus minMinus10_) Nat15) Int
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
serializeIn :
    Nat (ArgIn minLowerBound minUpperBound lowerBoundIfN_)
    -> Nat (ArgIn minUpperBound maxUpperBound upperBoundIfN_)
    ->
        ({ expected : Arr.Expectation
         , actual : { length : Nat (Min Nat0) }
         }
         -> error
        )
    -> Codec error element
    ->
        Codec
            error
            (Arr (In minLowerBound maxUpperBound) element)
serializeIn lowerBound upperBound toError serializeElement =
    Internal.serializeIn lowerBound
        upperBound
        (fromInternalError >> toError)
        serializeElement


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize `Arr`s with a specific amount of elements.

    import Serialize exposing (Codec)

    serializeGameRow :
        Codec
            String
            (Arr (Only Nat10) GameField)
    serializeGameRow =
        Arr.serialize nat10
            -- if we just want a simple error string
            Arr.errorToString
            serializeGameField

The encode/decode functions can be extracted if needed.

    encodeGameRow : Arr (Only Nat10) GameField -> Bytes
    encodeGameRow =
        Serialize.encodeToBytes serializeGameRow

    decodeGameRow :
        Bytes
        ->
            Result
                (Serialize.Error String)
                (Arr (Only Nat10) GameField)
    decodeGameRow =
        Serialize.decodeFromBytes serializeGameRow

-}
serialize :
    Nat (ArgIn min max ifN_)
    ->
        ({ expected : { length : Nat (Min Nat0) }
         , actual : { length : Nat (Min Nat0) }
         }
         -> error
        )
    -> Codec error element
    -> Codec error (Arr (In min max) element)
serialize length toError serializeElement =
    Internal.serialize length toError serializeElement



-- ## error


{-| An error for when the length of a decoded array is different from an exact expected length. You can transform it into

  - a message: [`errorToString`](InArr#errorToString)
  - an [`Arr.Error`](Arr#Error): [`generalizeError`](InArr#generalizeError)

See [`serialize`](InNat#serialize).

-}
type alias Error =
    { expected : { length : Nat (Min Nat0) }
    , actual : { length : Nat (Min Nat0) }
    }


{-| Convert its error to a [`Arr.Error`](Arr#Error): an error type that covers all cases where `Array` to `Arr` conversions can fail.

    import Serialize exposing (Codec)

    Serialize.mapError MinArr.generalizeError
    --> : Codec MinArr.Error a -> Codec Arr.Error a

Use this if you serialize `Arr (In X (XPlus a_))` together with `Arr (In A Z)`:

    Serialize.tuple
        (InArr.serialize nat10 ...)
        (InArr.serializeIn nat10 nat99 ...)
    --> error : `Codec`s have different custom errors

    Serialize.tuple
        (InArr.serialize nat10
            InArr.generalizeError ...
        )
        (InArr.serializeIn nat10 nat99
            identity ...
        )
    --> Codec
    -->     Arr.Error
    -->     ( Arr (Min Nat0) ...
    -->     , Arr (In Nat0 (Nat99Plus a_)) ...
    -->     )

Note: There's also [`errorToString`](MinArr#errorToString).

-}
generalizeError : Error -> Arr.Error
generalizeError error =
    error
        |> Common.generalizeError
            Arr.ExpectLength


{-| Convert an error into a readable message.

    { expected = { length = InNat.ExpectAtLeast nat11 }
    , actual = { length = nat10 }
    }
        |> MinArr.errorToString
    --> "expected an array of length >= 11 but the actual length was 10"

(example doesn't compile)

Equivalent to

    error
        |> generalizeError
        |> Common.errorToString

-}
errorToString : Error -> String
errorToString error =
    error
        |> generalizeError
        |> Arr.errorToString
