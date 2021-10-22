module InArr exposing
    ( push, removeAt, insertAt
    , intersperse
    , append, appendIn, prepend, prependIn
    , drop
    , isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost
    , serialize, serializeIn
    , Error, generalizeError, errorToString
    )

{-| If the length is `Only` or the maximum length is set to a specific value:

    -- only up to 50 tags
    tag : Arr (In min_ Nat50) String -> a -> Tagged a

use these operations instead of the ones in [`Arr`](Arr) or [`MinArr`](MinArr).


# modify

@docs push, removeAt, insertAt
@docs intersperse


## glue

@docs append, appendIn, prepend, prependIn


## part

@docs drop


# scan length

@docs isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost


# transform

@docs serialize, serializeIn


## error

@docs Error, generalizeError, errorToString

-}

import Arr exposing (Arr)
import Common exposing (fromInternalError)
import Internal
import LinearDirection exposing (LinearDirection)
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
    -> Arr (In minLength maxLength) element
    ->
        Arr
            (In
                (Nat1Plus minLength)
                (Nat1Plus maxLength)
            )
            element
push element =
    Internal.push element


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
    Internal.insertAt index direction element


{-| Place a value between all members.

To get the correct final length type, we need to give the current minimum and maximum length as an arguments.

    Arr.from3 "turtles" "turtles" "turtles"
        |> InArr.intersperse "on" nat3 nat3
    --> Arr.from5 "turtles" "on" "turtles" "on" "turtles"

-}
intersperse :
    element
    ->
        Nat
            (N
                minLength
                atLeastMinLength_
                (Is minLength To (Nat1Plus minDoubleLengthMinus1))
                minIs_
            )
    ->
        Nat
            (N
                maxLength
                atLeastMaxLength_
                (Is maxLength To (Nat1Plus maxDoubleLengthMinus1))
                maxIs_
            )
    -> Arr (In minLength maxLength) element
    -> Arr (In minDoubleLengthMinus1 maxDoubleLengthMinus1) element
intersperse separatorBetweenTheElements minLength maxLength =
    Internal.inIntersperse separatorBetweenTheElements minLength maxLength


{-| Attach elements of an `Arr` which has multiple possible amounts to the right.

    Arr.from3 1 2 3
        |> InArr.appendIn nat3 nat5
            between3And5Elements
    --> : Arr (In Nat6 (Nat8Plus a))

Use [`append`](InArr#append) if the appended `Arr` has an exact amount of elements.

-}
appendIn :
    Nat (N addedMin atLeastAddedMin_ (Is minLength To minLengthSum) addedMinIs_)
    -> Nat (N addedMax atLeastAddedMax_ (Is maxLength To maxLengthSum) addedMaxIs_)
    -> Arr (In addedMin addedMax) element
    -> Arr (In minLength maxLength) element
    -> Arr (In minLengthSum maxLengthSum) element
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
    Nat (N addedMin atLeastAddedMin_ (Is minLength To minLengthSum) addedMinIs_)
    -> Nat (N addedMax atLeastAddedMax_ (Is maxLength To maxLengthSum) addedMaxIs_)
    -> Arr (In addedMin addedMax) element
    -> Arr (In minLength maxLength) element
    -> Arr (In minLengthSum maxLengthSum) element
prependIn extensionMin extensionMax extension =
    Internal.prependIn extensionMin extensionMax extension


{-| Attach elements of an `Arr` with an exact amount of elements to the right.

    Arr.from3 1 2 3
        |> InArr.append nat3 (Arr.from3 4 5 6)
    --> Arr.from6 1 2 3 4 5 6

-}
append :
    Nat (N added atLeastAdded_ (Is minLength To minLengthSum) (Is maxLength To minSumMax))
    -> Arr (Only added) element
    -> Arr (In minLength maxLength) element
    -> Arr (In minLengthSum minSumMax) element
append addedLength arrExtension =
    Internal.inAppend addedLength arrExtension


{-| Add elements of an `Arr` with an exact amount of elements to the left.

    Arr.from3 1 2 3
        |> InArr.prepend nat3 (Arr.from3 4 5 6)
    --> Arr.from6 4 5 6 1 2 3

-}
prepend :
    Nat
        (N
            added
            atLeastAdded_
            (Is minLength To minLengthSum)
            (Is maxLength To maxLengthSum)
        )
    -> Arr (Only added) element
    -> Arr (In minLength maxLength) element
    -> Arr (In minLengthSum maxLengthSum) element
prepend addedLength arrExtension =
    Internal.inPrepend addedLength arrExtension


{-| Kick an element out of the `Arr` at a given index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    removeLast between1And10Elements =
        between1And10Elements
            |> InArr.removeAt nat0 LastToFirst

-}
removeAt :
    Nat (ArgIn indexMin_ minLengthMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minLengthMinus1) (Nat1Plus maxLengthMinus1)) element
    -> Arr (In minLengthMinus1 maxLengthMinus1) element
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
            (Is minTaken To minLength)
            (Is maxTaken To maxLength)
        )
    -> LinearDirection
    -> Arr (In minLength maxLength) element
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
            (Nat1Plus nMinus1)
            atLeastN
            (Is a_ To (Nat1Plus nPlusAMinus1))
            (Is valueToMax_ To maxLength)
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To minLength)
                    (Is minToValue_ To (Nat1Plus nMinus1))
                )
        }
    -> Arr (In minLength maxLength) element
    ->
        Nat.LessOrEqualOrGreater
            (Arr (In lowest nPlusAMinus1) element)
            (Arr (In (Nat1Plus nMinus1) atLeastN) element)
            (Arr (In (Nat2Plus nMinus1) maxLength) element)
isLength amount lowest =
    Internal.inIsLength amount lowest


{-| Compared to a range from a lower to an upper bound, is its length `BelowOrInOrAboveRange`?

`lowest` can be a number <= the minimum length.

    chooseFormation :
        Arr (In minLength_ Nat50) Character
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
                (Is upperBoundToMax_ To maxLength)
                upperBoundIs_
            )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To minLength)
                    (Is minToLowerBound_ To lowerBound)
                )
        }
    -> Arr (In minLength maxLength) element
    ->
        Nat.BelowOrInOrAboveRange
            (Arr (In lowest atLeastLowerBoundMinus1) element)
            (Arr (In lowerBound atLeastUpperBound) element)
            (Arr (In (Nat1Plus upperBound) maxLength) element)
isLengthInRange lowerBound upperBound lowest =
    Internal.inIsLengthInRange lowerBound upperBound lowest


{-| Is its length `BelowOrAtLeast` as big as a `Nat`?

`lowest` can be a number <= the minimum length.

    first5 :
        Arr (In minLength_ maxLength) element
        -> Maybe (Arr (In Nat5 maxLength) element)
    first5 arr =
        case arr |> MinArr.isLengthAtLeast nat5 { lowest = nat0 } of
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
            (Is atLeastRange_ To maxLength)
            is_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To minLength)
                    (Is (Nat1Plus lowestToLowerBound_) To lowerBound)
                )
        }
    -> Arr (In minLength maxLength) element
    ->
        Nat.BelowOrAtLeast
            (Arr (In lowest atLeastLowerBoundMinus1) element)
            (Arr (In lowerBound maxLength) element)
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
            (Is (Nat1Plus greaterRange_) To maxLength)
            is_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To minLength)
                    (Is minToUpperBound_ To upperBound)
                )
        }
    -> Arr (In minLength maxLength) element
    ->
        Nat.AtMostOrAbove
            (Arr (In lowest atLeastUpperBound) element)
            (Arr (In (Nat1Plus upperBound) maxLength) element)
isLengthAtMost upperBound lowest =
    Internal.inIsLengthAtMost upperBound lowest


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize `Arr`s within a minimum & maximum length.
To serialize `Arr`s of an exact length, use [`serialize`](InArr#serialize).

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
    Nat (ArgIn minLength maxLength ifN_)
    ->
        ({ expected : { length : Nat (Min Nat0) }
         , actual : { length : Nat (Min Nat0) }
         }
         -> error
        )
    -> Codec error element
    -> Codec error (Arr (In minLength maxLength) element)
serialize length toError serializeElement =
    Internal.serialize length toError serializeElement



-- ## error


{-| An error for when the length of a decoded array is different from an exact expected length. You can transform it into

  - a message: [`errorToString`](InArr#errorToString)
  - an [`Arr.Error`](Arr#Error): [`generalizeError`](InArr#generalizeError)

See [`serialize`](InArr#serialize).

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
        (InArr.serializeIn nat10 nat20
            identity ...
        )
    --> Codec
    -->     Arr.Error
    -->     ( Arr (In Nat10 (Nat10Plus a_)) ...
    -->     , Arr (In Nat10 (Nat20Plus b_)) ...
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
    --> "Expected an array of length >= 11, but the actual length was 10."

(example doesn't compile)

Equivalent to

    error
        |> InArr.generalizeError
        |> Arr.errorToString

-}
errorToString : Error -> String
errorToString error =
    error
        |> generalizeError
        |> Arr.errorToString
