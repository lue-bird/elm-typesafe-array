module Common exposing (fromInternalError, generalizeError)

{-| Functions that are used in many modules but shouldn't be exposed in `Arr`.
-}

import Arr
import Internal
import Nat exposing (Min, Nat)
import Nats exposing (Nat0)


generalizeError :
    (expectedLength -> Arr.Expectation)
    ->
        { expected : { length : expectedLength }
        , actual : { length : Nat (Min Nat0) }
        }
    ->
        { expected : Arr.Expectation
        , actual : { length : Nat (Min Nat0) }
        }
generalizeError generalizeExpectedLength error =
    { expected = generalizeExpectedLength error.expected.length
    , actual = error.actual
    }


fromInternalError :
    { expected : Internal.Expectation
    , actual : { length : Nat (Min Nat0) }
    }
    -> Arr.Error
fromInternalError error =
    { expected =
        fromInternalExpectation error.expected
    , actual = error.actual
    }


fromInternalExpectation : Internal.Expectation -> Arr.Expectation
fromInternalExpectation internalExpectation =
    case internalExpectation of
        Internal.LengthInBound inBound ->
            Arr.LengthInBound inBound

        Internal.ExpectLength length ->
            Arr.ExpectLength length
