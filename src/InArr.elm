module InArr exposing
    ( push, extend, extendOnly, removeAt, insertAt
    , isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost
    , serialize
    )

{-| If the maximum length is set to a specific value (also in `ArgOnly`),

    -- only up to 50 tags
    tag :
        Arr (In min Nat50 maybeN) String
        -> a
        -> Tagged a

use these operations instead of the ones in `Arr` or `MinArr`.


## modify

@docs push, extend, extendOnly, removeAt, insertAt


## scan length

@docs isLengthInRange, isLength, isLengthAtLeast, isLengthAtMost


## extra

@docs serialize

-}

import Arr exposing (Arr, fromArray, length, toArray)
import Internal.InArr as Internal
import LinearDirection exposing (LinearDirection(..))
import MinArr
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


{-| Use `MinArr.push`, if the `max` isn't known to be a certain value.
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
-}
extend :
    Arr (In extensionMin extensionMax) element
    -> Nat (ArgN extensionMin (Is min To extendedMin) x)
    -> Nat (ArgN extensionMax (Is max To extendedMax) y)
    -> Arr (In min max) element
    -> Arr (In extendedMin extendedMax) element
extend extension extensionMin extensionMax =
    Internal.extend extension extensionMin extensionMax


{-| Append the elements of an `Arr (Only ...)`.
-}
extendOnly :
    Nat (ArgN added (Is min To sumMin) (Is max To sumMax))
    -> Arr (Only added) element
    -> Arr (In min max) element
    -> Arr (In sumMin sumMax) element
extendOnly arrExtension =
    Internal.extendOnly arrExtension


{-| Kick an element out of an Arr at a given index in a direction.
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


{-| Compare the length to an exact `Nat (ArgN ...)` length.
Are there `more`, `less` or an `equal` amount of elements?

`min` ensures that the `Nat (ArgN ...)` is greater than the minimum length.

    chooseFormation :
        Arr (In Nat0 Nat50) Character
        -> Formation
    chooseFormation characters =
        characters
            |> InArr.isLength nat7
                { min = nat0 }
                { equal = SpecialAttack
                , less = Retreat
                , more = Fight
                }

    type Formation
        = SpecialAttack (Arr (Only Nat7) Character)
        | Retreat (Arr (In Nat0 Nat6) Character)
        | Fight (Arr (In Nat8 Nat50) Character)

-}
isLength :
    Nat
        (ArgN
            tried
            (Is triedToMax To max)
            (Is a To (Nat1Plus atLeastTriedMinus1))
        )
    -> { min : Nat (ArgN min (Is minToTried To tried) x) }
    ->
        { equal :
            Arr (Only tried) element
            -> result
        , greater :
            Arr (In (Nat2Plus triedMinus1) max) element -> result
        , less :
            Arr (In min atLeastTriedMinus1) element -> result
        }
    -> Arr (In min max) element
    -> result
isLength tried min cases =
    Internal.isLength tried min cases


{-| Compared to a range from a lower to an upper bound, is the length

  - `inRange`

  - `greater` than the upper bound or

  - `less` than the lower bound?

`min` ensures that the lower bound is greater than the minimum length.

    chooseFormation :
        Arr (In Nat0 Nat50) Character
        -> Formation
    chooseFormation characters =
        characters
            |> InArr.isLengthInRange nat10
                nat20
                { min = nat0 }
                { equal = SpecialAttack
                , less = Retreat
                , more = Fight
                }

    type Formation
        = SpecialAttack (Arr (In Nat10 Nat20) Character)
        | Retreat (Arr (In Nat0 Nat9) Character)
        | Fight (Arr (In Nat21 Nat50) Character)

-}
isLengthInRange :
    Nat
        (ArgN
            lowerBound
            (Is lowerBoundToUpperBound To upperBound)
            (Is lowerBoundA To (Nat1Plus atLeastLowerBoundMinus1))
        )
    -> Nat (ArgN upperBound (Is upperBoundToMax To max) (Is upperBoundA To atLeastUpperBound))
    -> { min : Nat (ArgN min (Is minToLowerBound To lowerBound) x) }
    ->
        { inRange :
            Arr (In lowerBound atLeastUpperBound) element
            -> result
        , less :
            Arr (In min atLeastLowerBoundMinus1) element
            -> result
        , greater :
            Arr (In (Nat1Plus upperBound) max) element
            -> result
        }
    -> Arr (In min max) element
    -> result
isLengthInRange lowerBound upperBound min cases =
    Internal.isLengthInRange lowerBound upperBound min cases


{-| Is the length

  - `equalOrGreater` than a lower bound or

  - `less`?

`min` ensures that the lower bound is greater than the minimum length.

    first5 :
        Arr (In min max maybeN) element
        -> Maybe (Arr (In Nat5 max maybeN) element)
    first5 =
        MinArr.lowerMinLength nat0
            >> MinArr.isLengthAtLeast nat5
                { min = nat0 }
                { less = \_ -> Nothing
                , equalOrGreater = Just
                }

-}
isLengthAtLeast :
    Nat
        (ArgN
            lowerBound
            (Is a To (Nat1Plus atLeastTriedMinus1))
            (Is atLeastRange To max)
        )
    -> { min : Nat (ArgN min (Is (Nat1Plus lessRange) To lowerBound) x) }
    ->
        { less :
            Arr (In min atLeastTriedMinus1) element
            -> result
        , equalOrGreater :
            Arr (In lowerBound max) element
            -> result
        }
    -> Arr (In min max) element
    -> result
isLengthAtLeast lowerBound cases =
    Internal.isLengthAtLeast lowerBound cases


{-| Is the length

  - `equalOrLess` than a upper bound or

  - `greater`?

`min` ensures that the upper bound is greater than the minimum length.

    -- at least 3 and only up to 50 tags
    tag :
        Arr (In (Nat3Plus orHigherMin) Nat50 maybeN) String
        -> a
        -> Tagged a

    tagIfValidTags :
        Arr (In (Nat3Plus orHigherMin) max maybeN)
        -> a
        -> Maybe (Tagged a)
    tagIfValidTags array value =
        array
            |> Arr.fromArray
            |> InArr.isLengthAtMost nat50
                { equalOrLess = tag value >> Just
                , greater = \_ -> Nothing
                }

-}
isLengthAtMost :
    Nat
        (ArgN
            upperBound
            (Is a To atLeastUpperBound)
            (Is (Nat1Plus greaterRange) To max)
        )
    -> { min : Nat (ArgN min (Is minToUpperBound To upperBound) x) }
    ->
        { equalOrLess : Arr (In min atLeastUpperBound) element -> result
        , greater : Arr (In (Nat1Plus upperBound) max) element -> result
        }
    -> Arr (In min max) element
    -> result
isLengthAtMost upperBound min cases =
    Internal.isLengthAtMost upperBound min cases


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize `Arr`s within a minimum & maximum length.

    import Serialize

    serialize10To15Ints :
        Serialize.Codec
            String
            (Arr (In Nat10 (Nat15Plus a)) Int)
    serialize10To15Ints =
        InArr.serialize Serialize.int nat10 nat15

    encode :
        Arr (In (Nat10Plus orHigherMin) Nat15 maybeN) Int
        -> Bytes
    encode =
        InNat.value
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
    Nat (ArgIn minLowerBound upperBound lowerBoundMaybeN)
    -> Nat (ArgIn upperBound upperBoundPlusA upperBoundMaybeN)
    -> Serialize.Codec String element
    ->
        Serialize.Codec
            String
            (Arr (In minLowerBound upperBoundPlusA) element)
serialize lowerBound upperBound serializeElement =
    Internal.serialize lowerBound upperBound serializeElement
