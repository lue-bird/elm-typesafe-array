module Arr exposing
    ( Arr
    , fromArray, repeat, nats, random
    , empty, from1, from2, from3, from4, from10, from11, from12, from13, from14, from15, from16, from5, from6, from7, from8, from9
    , length, at
    , take, drop
    , map, fold, toArray, map2, map3, map4, foldWith, reverse
    , replaceAt
    , lowerMinLength
    , restoreMaxLength
    , serialize
    )

{-| An `Arr` describes an array where you know more about the amount of elements.

    Array.empty |> Array.get 0
    --> Nothing

    Arr.empty |> Arr.at nat0 FirstToLast
    --> compile time error

Is this any useful? Let's look at an example:

> 0 to 100 joined by a space

    joinBy between =
        \before after -> before ++ between ++ after

    let
        intStringsAfter =
            Array.fromList (List.range 1 100)
                String.fromInt
    in
    Array.foldl (joinBy " ") "0" intStringsAfter
    --> "0 1 2 3 4 5 ..."

    Arr.nats nat100
        |> Arr.map (val >> String.fromInt)
        |> Arr.foldWith FirstToLast (joinBy " ")
    --> "0 1 2 3 4 5 ..."

The `Array` version just seems hacky and is less readable. `Arr` simply knows more about the length at compile time, so you can e.g. call `foldWith` without a worry.

@docs Arr


## create

@docs fromArray, repeat, nats, random


### exact

@docs empty, from1, from2, from3, from4, from10, from11, from12, from13, from14, from15, from16, from5, from6, from7, from8, from9


## scan

@docs length, at


## part

@docs take, drop


## transform

@docs map, fold, toArray, map2, map3, map4, foldWith, reverse


### modify

@docs replaceAt


## drop information

@docs lowerMinLength


## restore information

@docs restoreMaxLength


## extra

@docs serialize

-}

import Arguments exposing (..)
import Array exposing (Array)
import Array.LinearDirection as Array
import InNat
import Internal.Arr as Internal
import LinearDirection exposing (LinearDirection(..))
import NNat exposing (..)
import NNats exposing (nat0, nat1)
import Nat exposing (ArgIn, ArgN, In, Is, Min, N, Nat, Only, To)
import Random
import Serialize
import TypeNats exposing (..)
import Typed exposing (Checked, Internal, Typed)


{-| An `Arr` describes an array where you know more about the amount of elements.


## value / return types

  - amount >= 5

```
Arr (Min Nat5) ...
```

  - 2 <= amount <= 12

```
Arr (In Nat2 Nat12) ...
```

  - = 4

```
Arr (Only Nat4) ...
```

  - the exact amount 3, also described as the difference between 2 numbers

```
Arr
    (N Nat3
        (Nat3Plus more)
        (Is a To (Nat3Plus a))
        (Is b To (Nat3Plus b))
    )
```


## function argument types

  - amount >= 4

```
Arr (In (Nat4Plus minMinus4) max) ...
```

  - 4 <= amount <= 15

```
Arr (In (Nat4Plus minMinus4) Nat15) ...
```

  - = 15

```
Arr (Only Nat15) ...
```

  - any amount

```
Arr range ...
```

-}
type alias Arr length element =
    Typed
        Checked
        Internal.ArrTag
        Internal
        (Internal.Content length element)


{-| Convert the `Arr` to an `Array`.
Just do this in the end.
Try to keep extra information as long as you can.

    Arr.nats nat5
        |> Arr.map val
        |> Arr.toArray
    --> Array.fromList [ 0, 1, 2, 3, 4 ]

-}
toArray : Arr length element -> Array element
toArray =
    Internal.toArray



-- ## create


{-| An `Arr` with a given amount of same elements.

    Arr.repeat nat4 'L'
    --> Arr.from4 'L' 'L' 'L' 'L'
    --> : Arr (N Nat4 ...) Char


    Arr.repeat atLeast3 'L'
    --> : Arr (Min Nat3) Char

-}
repeat :
    Nat (ArgIn min max maybeN)
    -> element
    -> Arr (In min max) element
repeat amount element =
    Internal.repeat amount element


{-| Create an `Arr` from an `Array`. As every `Array` has `>= 0` elements:

    Arr.fromArray arrayFromSomewhere
    --> : Arr (Min Nat0)

    Arr.fromArray
        (Array.fromList [ 0, 1, 2, 3, 4, 5, 6 ])
    --> big no

    Arr.from7 0 1 2 3 4 5 6
    --> ok

    Arr.nats nat7
    --> big yes

Tell the compiler if you know the amount of elements. Make sure the it knows as much as you!

-}
fromArray : Array element -> Arr (Min Nat0) element
fromArray =
    Internal.fromArray


{-| No elements.

    Arr.empty
    --> : Arr (N Nat0 ...) element
        |> NArr.push ":)"
    --> : Arr (N Nat1 ...) String

-}
empty : Arr (Only Nat0) element
empty =
    Internal.empty


{-| Create an `Arr (Only Nat1)` from exactly 1 elements in this order.
-}
from1 : element -> Arr (Only Nat1) element
from1 =
    \a -> empty |> push a


{-| Create an `Arr (Only Nat2)` from exactly 2 elements in this order.
-}
from2 : element -> element -> Arr (Only Nat2) element
from2 =
    apply1 from1 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat3)` from exactly 3 elements in this order.
-}
from3 : element -> element -> element -> Arr (Only Nat3) element
from3 =
    apply2 from2 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat4)` from exactly 4 elements in this order.
-}
from4 :
    element
    -> element
    -> element
    -> element
    -> Arr (Only Nat4) element
from4 =
    apply3 from3 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat5)` from exactly 5 elements in this order.
-}
from5 :
    element
    -> element
    -> element
    -> element
    -> element
    -> Arr (Only Nat5) element
from5 =
    apply4 from4 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat6)` from exactly 6 elements in this order.
-}
from6 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (Only Nat6) element
from6 =
    apply5 from5 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat7)` from exactly 7 elements in this order.
-}
from7 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (Only Nat7) element
from7 =
    apply6 from6 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat8)` from exactly 8 elements in this order.
-}
from8 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (Only Nat8) element
from8 =
    apply7 from7 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat9)` from exactly 9 elements in this order.
-}
from9 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (Only Nat9) element
from9 =
    apply8 from8 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat10)` from exactly 10 elements in this order.
-}
from10 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (Only Nat10) element
from10 =
    apply9 from9 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat11)` from exactly 11 elements in this order.
-}
from11 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (Only Nat11) element
from11 =
    apply10 from10 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat12)` from exactly 12 elements in this order.
-}
from12 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (Only Nat12) element
from12 =
    apply11 from11 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat13)` from exactly 13 elements in this order.
-}
from13 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (Only Nat13) element
from13 =
    apply12 from12 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat14)` from exactly 14 elements in this order.
-}
from14 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (Only Nat14) element
from14 =
    apply13 from13 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat15)` from exactly 15 elements in this order.
-}
from15 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (Only Nat15) element
from15 =
    apply14 from14 (\init -> \last -> init |> push last)


{-| Create an `Arr (Only Nat16)` from exactly 16 elements in this order.
-}
from16 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (Only Nat16) element
from16 =
    apply15 from15 (\init -> \last -> init |> push last)



-- ## modify


push :
    element
    -> Arr (In min max) element
    -> Arr (In (Nat1Plus min) (Nat1Plus max)) element
push element =
    Internal.push element (InNat.addN nat1)


{-| Set the element at an index in a direction.

    Arr.from3 "I" "am" "ok"
        |> Arr.replaceAt nat2 FirstToLast "confusion"
    --> Arr.from3 "I" "am" "confusion"

    Arr.from3 "I" "am" "ok"
        |> Arr.replaceAt nat1 LastToFirst "feel"
    --> Arr.from3 "I" "feel" "ok"

-}
replaceAt :
    Nat (ArgIn indexMin minLengthMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minLengthMinus1) max) element
    -> Arr (In (Nat1Plus minLengthMinus1) max) element
replaceAt index direction replacingElement =
    Internal.replaceAt index direction replacingElement



-- ## part


{-| This works for both

    Arr.from8 0 1 2 3 4 5 6 7
        |> Arr.take nat3 nat3 LastToFirst
    --> Arr.from3 5 6 7

    Arr.from8 0 1 2 3 4 5 6 7
        |> Arr.take between3To7 nat7 FirstToLast
    --> : Arr (In Nat3 Nat7) ...

-}
take :
    Nat (ArgIn minTaken maxTaken takenMaybeN)
    -> Nat (ArgN maxTaken (Is a To atLeastMaxTaken) (Is maxTakenToMin To min))
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken atLeastMaxTaken) element
take takenAmount maxAmount direction =
    Internal.take takenAmount maxAmount direction


{-| After a certain number of elements from one side.

    with6To10Elements
        |> Arr.drop nat2 LastToFirst
    --> : Arr (In Nat4 (Nat10Plus a)) ...

Use `take` `LastToFirst` if you know the exact amount of elements.

-}
drop :
    Nat (ArgN dropped (Is minTaken To min) (Is maxTaken To max))
    -> LinearDirection
    -> Arr (In min max) element
    -> Arr (In minTaken maxTaken) element
drop droppedAmount direction =
    Internal.drop droppedAmount direction



-- ## transform


{-| Change every element.

    aToZ =
        Arr.nats nat26
            |> Arr.map
                ((+) ('a' |> Char.toCode)
                    >> Char.fromCode
                )

-}
map :
    (aElement -> bElement)
    -> Arr length aElement
    -> Arr length bElement
map alter =
    Internal.map alter


{-| Combine the elements of 2 `Arr`s to a new element.
If one list is longer, the extra elements are dropped.

    teamLifes aBoard bBoard =
        Arr.map2 (\a b -> a.lifes + b.lifes)
            aBoard
            bBoard

-}
map2 :
    (a -> b -> combined)
    -> Arr length a
    -> Arr length b
    -> Arr length combined
map2 combine aArr bArr =
    Internal.map2 combine aArr bArr


{-| Works like [map2](Arr#map2).
-}
map3 :
    (a -> b -> c -> combinedElement)
    -> Arr length a
    -> Arr length b
    -> Arr length c
    -> Arr length combinedElement
map3 combine aArr bArr cArr =
    map2 (\f c -> f c)
        (map2 combine aArr bArr)
        cArr


{-| Works like [map2](Arr#map2).
-}
map4 :
    (a -> b -> c -> d -> combinedElement)
    -> Arr length a
    -> Arr length b
    -> Arr length c
    -> Arr length d
    -> Arr length combinedElement
map4 combine aArr bArr cArr dArr =
    map2 (\f c -> f c)
        (map3 combine aArr bArr cArr)
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
    -> Arr (In min max) element
    -> result
fold direction reduce initial =
    toArray >> Array.fold direction reduce initial


{-| A fold where the initial result is the first value.

    Arr.foldWith FirstToLast maximum
        (Arr.from3 234 345 543)
    --> 543

-}
foldWith :
    LinearDirection
    -> (element -> element -> element)
    -> Arr (In (Nat1Plus minMinus1) max) element
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
    Nat (ArgIn indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) max) element
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



-- ## drop information


{-| Use a lower minimum length in the type.

    [ atLeast3Elements
    , atLeast4Elements
    ]

Elm complains:

> But all the previous elements in the list are
> `Arr (Min Nat3) ...`

    [ atLeast3Elements
    , atLeast4Elements
        |> Arr.lowerMinLength nat3
    ]

-}
lowerMinLength :
    Nat (ArgIn lowerMin min lowerMaybeN)
    -> Arr (In min max) element
    -> Arr (In lowerMin max) element
lowerMinLength =
    Internal.lowerMinLength



-- ## restore information


{-| Make an `Arr` with a fixed maximum length fit into functions with require a higher maximum length.

While designing argument annotations as general as possible:

    atMost18Elements : Arr (In min Nat18) ...

The argument in `atMost18Elements` should also fit in `atMost19Elements`.

    atMost19Elements theArgument
    --> error :(

    atMost19Elements
        (theArgument |> Arr.restoreMaxLength nat18)
    --> works

-}
restoreMaxLength :
    Nat (ArgN max (Is a To atLeastMax) x)
    -> Arr (In min max) element
    -> Arr (In min atLeastMax) element
restoreMaxLength maximumLength =
    Internal.restoreMaxLength maximumLength



-- ## create


{-| Increasing natural numbers. In the end, there are `length` numbers.

    Arr.nats nat10
    --> : Arr
    -->     (N Nat10 ...)
    -->     (Nat (In Nat0 (Nat9Plus a)))

    from first length_ =
        Arr.nats length_
            |> Arr.map (InNat.add first)

-}
nats :
    Nat
        (ArgIn
            (Nat1Plus minLengthMinus1)
            (Nat1Plus maxLengthMinus1)
            lengthMaybeN
        )
    ->
        Arr
            (In
                (Nat1Plus minLengthMinus1)
                (Nat1Plus maxLengthMinus1)
            )
            (Nat (In Nat0 maxLengthMinus1))
nats length_ =
    Internal.nats length_


{-| Generate a given `amount` of elements and put them in an `Arr`.
-}
random :
    Nat length
    -> Random.Generator element
    -> Random.Generator (Arr length element)
random amount generateElement =
    Internal.random amount generateElement


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize `Arr`s with a specific amount of elements.

    import Serialize


    -- we can't start if we have no worlds to choose from!
    serializeGameRow :
        Serialize.Codec
            String
            (Arr (N Nat10 ...) GameField)
    serializeGameRow =
        Arr.serialize nat10 serializeGameField

    encode : Arr (Only Nat10 maybeN) GameField -> Bytes
    encode =
        Arr.restoreLength nat10
            >> Serialize.encodeToBytes serializeGameRow

    decode :
        Bytes
        ->
            Result
                (Serialize.Error String)
                (Arr (N Nat10 ...) GameField)
    decode =
        Serialize.decodeFromBytes serializeGameRow

-}
serialize :
    Nat (ArgIn min max maybeN)
    -> Serialize.Codec String element
    -> Serialize.Codec String (Arr (In min max) element)
serialize length_ serializeElement =
    Internal.serialize length_ serializeElement
