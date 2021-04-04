module Arr exposing
    ( Arr
    , length, at
    , takeIn, takeN
    , empty, from1, from2, from3, from4, fromArray, repeat
    , map, fold, toArray, combine2, combine3, combine4, foldWith
    , reverse, random, SerializeError(..)
    )

{-|

@docs Arr


## scan

@docs length, at


## part

@docs takeIn, takeN


## create

@docs empty, from1, from2, from3, from4, fromArray, repeat


## transform

@docs map, fold, toArray, combine2, combine3, combine4, foldWith


## extra

@docs reverse, random, SerializeError

-}

import Array exposing (Array)
import Internal.Arr as Internal
import LinearDirection exposing (LinearDirection(..))
import LinearDirection.Array as Array
import N.Arguments exposing (..)
import NNat exposing (..)
import NNats exposing (nat0, nat3)
import Nat exposing (Nat)
import Nat.Bound exposing (..)
import Random
import TypeNats exposing (..)


type alias Arr length element =
    Internal.Arr length element


toArray : Arr length element -> Array element
toArray =
    Internal.toArray



-- ## create


{-| A `Arr` with a given amount of same elements.

    Arr.repeat nat4 'L'
    --> Arr.from4 'L' 'L' 'L' 'L'
    --> of type Arr (N Nat4 ...) Char


    Arr.repeat atLeast3 'L'
    --> is of type Arr (Min Nat3) Char

-}
repeat :
    Nat length
    -> element
    -> Arr length element
repeat amount element =
    Internal.repeat amount element


{-| Every `Array` has `>= 0` elements → is an `Arr (Min Nat0)`.
-}
fromArray : Array element -> Arr (ValueMin Nat0) element
fromArray =
    Internal.fromArray


{-| No elements.

    Arr.empty
    --> is of type Arr (In Nat0 max) element
        |> Arr.push ":)"
    --> is of type Arr (N Nat4 ...) String

-}
empty : Arr (N Nat0 (Is a To) a (And b To b)) element
empty =
    Internal.empty


{-| Create a `Arr (N Nat1 ...)` from exactly 1 element in this order.
Short for `Arr.empty |> Arr.push`
-}
from1 :
    element
    ->
        Arr
            (N
                (Nat1Plus Nat0)
                (Is b To)
                (Nat1Plus b)
                (And nPlusB To (Nat1Plus nPlusB))
            )
            element
from1 =
    \last -> empty |> push last


from2 :
    element
    -> element
    ->
        Arr
            (N
                Nat2
                (Is a To)
                (Nat2Plus a)
                (And b To (Nat1Plus (Nat1Plus b)))
            )
            element
from2 =
    apply1 from1 (\init -> \last -> init |> push last)


from3 :
    element
    -> element
    -> element
    ->
        Arr
            (N
                Nat3
                (Is a To)
                (Nat3Plus a)
                (And b To (Nat3Plus b))
            )
            element
from3 =
    apply2 from2 (\init -> \last -> init |> push last)


from4 :
    element
    -> element
    -> element
    -> element
    ->
        Arr
            (N
                Nat4
                (Is a To)
                (Nat4Plus a)
                (And b To (Nat4Plus b))
            )
            element
from4 =
    apply3 from3 (\init -> \last -> init |> push last)



-- ## modify


push :
    element
    -> Arr (N n (Is a To) nPlusA (And b To nPlusB)) element
    ->
        Arr
            (N
                (Nat1Plus n)
                (Is a To)
                (Nat1Plus nPlusA)
                (And b To (Nat1Plus nPlusB))
            )
            element
push element =
    Internal.nPush element



-- ## part


takeN :
    ( Nat (N taken (Is dropped To) min x)
    , Nat (N taken isATo takenPlusA bDiff)
    )
    -> LinearDirection
    -> Arr (In min max maybeN) element
    -> Arr (N taken isATo takenPlusA bDiff) element
takeN amount direction =
    Internal.takeN amount direction


take3 :
    LinearDirection
    -> Arr (In (Nat3Plus a) max maybeN) element
    -> Arr (N Nat3 (Is Nat0 To) Nat3 (And b To (Nat3Plus b))) element
take3 =
    takeIn nat3 nat3


{-| This works somewhat magically.
-}
takeIn :
    Nat (In minTaken maxTaken takenMaybeN)
    -> Nat (N maxTaken (Is maxTakenToMin To) min x)
    -> LinearDirection
    -> Arr (In min max maybeN) element
    -> Arr (In minTaken maxTaken takenMaybeN) element
takeIn amount maxAmount direction =
    Internal.takeIn amount maxAmount direction



-- ## transform


map :
    (aElement -> bElement)
    -> Arr length aElement
    -> Arr length bElement
map alter =
    Internal.map alter


{-| At each index of 2 `InArr`s,
map the elements into a new element.
If one list is longer, the extra elements are dropped.

    teamLifes aBoard bBoard =
        InArr.combine2
            (\a b -> a.lifes + b.lifes)
            aBoard
            bBoard

-}
combine2 :
    (a -> b -> combined)
    -> Arr length a
    -> Arr length b
    -> Arr length combined
combine2 combine aArr bArr =
    Internal.combine2 combine aArr bArr


{-| Works like [combine2](Arr#combine2).
-}
combine3 :
    (a -> b -> c -> combinedElement)
    -> Arr length a
    -> Arr length b
    -> Arr length c
    -> Arr length combinedElement
combine3 combine aArr bArr cArr =
    combine2 (\f c -> f c)
        (combine2 combine aArr bArr)
        cArr


{-| Works like [combine2](Arr#combine2).
-}
combine4 :
    (a -> b -> c -> d -> combinedElement)
    -> Arr length a
    -> Arr length b
    -> Arr length c
    -> Arr length d
    -> Arr length combinedElement
combine4 combine aArr bArr cArr dArr =
    combine2 (\f c -> f c)
        (combine3 combine aArr bArr cArr)
        dArr


{-| Reduce an `Arr` in a [direction](https://package.elm-lang.org/packages/indique/elm-linear-direction/latest/).

    Arr.from5 "l" "i" "v" "e"
        |> Arr.fold FirstToLast (++) ""
    --> "live"

    Arr.from5 "l" "i" "v" "e"
        |> Arr.fold LastToFirst (++) ""
    --> "evil"

-}
fold :
    LinearDirection
    -> (element -> result -> result)
    -> result
    -> Arr (In min max maybeExact) element
    -> result
fold direction reduce initial =
    toArray >> Array.fold direction reduce initial


foldWith :
    LinearDirection
    -> (element -> element -> element)
    -> Arr (In (Nat1Plus minMinus1) max maybeN) element
    -> element
foldWith direction reduce =
    \inArr ->
        Array.fold direction
            reduce
            (at nat0 direction inArr)
            (Array.removeAt 0
                (LinearDirection.opposite direction)
                (inArr |> toArray)
            )



-- ## scan


{-| The amount of elements.

    lastIndex =
        length >> NNat.sub1

-}
length : Arr length element -> Nat length
length =
    Internal.length


{-| Element at a valid position.

    Arr.from3 1 2 3
        |> Arr.at nat1
    --> 2

-}
at :
    Nat (In indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) max maybeN) element
    -> element
at index direction =
    Internal.at index direction



-- ## extra


{-| Flip the order of the elements.

    Arr.from5 "l" "i" "v" "e"
        |> Arr.reverse
    --> Arr.from5 "e" "v" "i" "l"

-}
reverse : Arr length element -> Arr length element
reverse =
    Internal.reverse



-- ## create


range :
    Nat (In minFirst maxFirst firstMaybeN)
    -> Nat (N maxFirst (Is maxFirstToMinLast To) minLast y)
    -> Nat (In minLast maxLast lastMaybeN)
    -> Arr (ValueMin (Nat1Plus maxFirstToMinLast)) (Nat (ValueMin minFirst))
range first firstMax last =
    Internal.range first firstMax last


random :
    Nat length
    -> Random.Generator element
    -> Random.Generator (Arr length element)
random amount generateElement =
    Internal.random amount generateElement



-- ## extra


{-| What could go wrong when decoding a `Arr`.
-}
type SerializeError elementError
    = ElementSerializeError elementError
    | WrongAmountSerializeError
