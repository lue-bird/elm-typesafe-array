module Arr exposing
    ( Arr
    , fromArray, repeat, nats, minNats, random
    , empty, from1
    , from2, from3, from4, from5, from6, from7, from8, from9, from10, from11, from12, from13, from14, from15, from16
    , length, at
    , replaceAt, updateAt, takeWhen, dropWhen
    , take, takeMax, groupsOf
    , map, fold, toArray, foldWith, reverse, resize, values
    , map2, map3, map4
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


# create

@docs fromArray, repeat, nats, minNats, random


## exact

@docs empty, from1

[Skip to `from16`](Arr#from16).

@docs from2, from3, from4, from5, from6, from7, from8, from9, from10, from11, from12, from13, from14, from15, from16


# scan

@docs length, at


# modify

@docs replaceAt, updateAt, takeWhen, dropWhen


## part

@docs take, takeMax, groupsOf


# transform

@docs map, fold, toArray, foldWith, reverse, resize, values


## combine

@docs map2, map3, map4


## drop information

@docs lowerMinLength


## restore information

@docs restoreMaxLength


# extra

@docs serialize

-}

import Arguments exposing (..)
import Array exposing (Array)
import Array.LinearDirection as Array
import Internal as Internal exposing (inPush)
import LinearDirection exposing (LinearDirection(..))
import NNat exposing (..)
import NNats exposing (nat0)
import Nat exposing (ArgIn, In, Is, Min, N, Nat, To)
import Random
import Serialize
import TypeNats exposing (..)
import Typed exposing (Checked, Internal, Typed)


{-| An `Arr` describes an array where you know more about the amount of elements.


## types

  - amount >= 5

```
Arr (Min Nat5) ...
```

  - 2 <= amount <= 12

```
Arr (In Nat2 (Nat12Plus a_)) ...
```


### as argument types

  - = 15

```
Arr (Only Nat15) ...
```

  - amount >= 4

```
Arr (In (Nat4Plus minMinus4_) max_) ...
```

  - 4 <= amount <= 15

```
Arr (In (Nat4Plus minMinus4_) Nat15) ...
```


## as storage types

For example to store in your `Model`

  - = 15

```
Arr (Only Nat15) ...
```

  - amount >= 4

```
Arr (Min Nat4) ...
```

  - 4 <= amount <= 15

```
Arr (In Nat4 Nat15) ...
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
toArray : Arr length_ element -> Array element
toArray =
    Internal.toArray



-- ## create


{-| An `Arr` with a given amount of same elements.

    Arr.repeat nat4 'L'
    --> Arr.from4 'L' 'L' 'L' 'L'
    --> : Arr (In Nat4 (Nat4Plus a)) Char


    Arr.repeat atLeast3 'L'
    --> : Arr (Min Nat3) Char

-}
repeat :
    Nat (ArgIn min max ifN_)
    -> element
    -> Arr (In min max) element
repeat amount element =
    Internal.repeat amount element


{-| Create an `Arr` from an `Array`. As every `Array` has `>= 0` elements:

    Arr.fromArray arrayFromSomewhere
    --> : Arr (Min Nat0)

Don't use it this way:

    Arr.fromArray
        (Array.fromList [ 0, 1, 2, 3, 4, 5, 6 ])
    --> big no

Tell the compiler if you know the amount of elements. Make sure the it knows as much as you!

    Arr.from7 0 1 2 3 4 5 6
    --> ok

    Arr.nats nat7
    --> big yes

-}
fromArray : Array element -> Arr (Min Nat0) element
fromArray =
    Internal.fromArray


{-| No elements.

    Arr.empty
    --> : Arr (In Nat0 atLeast0) element
        |> InArr.push ":)"
    --> : Arr (In Nat1 (Nat1Plus atLeast0)) String

-}
empty : Arr (In Nat0 atLeast0_) element_
empty =
    Internal.empty


{-| Create an `Arr` with exactly 1 element.
-}
from1 : element -> Arr (In Nat1 (Nat1Plus a_)) element
from1 =
    \a -> empty |> inPush a


{-| Create an `Arr` with exactly 2 elements in this order.
-}
from2 : element -> element -> Arr (In Nat2 (Nat2Plus a_)) element
from2 =
    apply1 from1 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 3 elements in this order.
-}
from3 :
    element
    -> element
    -> element
    -> Arr (In Nat3 (Nat3Plus a_)) element
from3 =
    apply2 from2 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 4 elements in this order.
-}
from4 :
    element
    -> element
    -> element
    -> element
    -> Arr (In Nat4 (Nat4Plus a_)) element
from4 =
    apply3 from3 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 5 elements in this order.
-}
from5 :
    element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat5 (Nat5Plus a_)) element
from5 =
    apply4 from4 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 6 elements in this order.
-}
from6 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat6 (Nat6Plus a_)) element
from6 =
    apply5 from5 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 7 elements in this order.
-}
from7 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat7 (Nat7Plus a_)) element
from7 =
    apply6 from6 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 8 elements in this order.
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
    -> Arr (In Nat8 (Nat8Plus a_)) element
from8 =
    apply7 from7 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 9 elements in this order.
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
    -> Arr (In Nat9 (Nat9Plus a_)) element
from9 =
    apply8 from8 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 10 elements in this order.
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
    -> Arr (In Nat10 (Nat10Plus a_)) element
from10 =
    apply9 from9 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 11 elements in this order.
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
    -> Arr (In Nat11 (Nat11Plus a_)) element
from11 =
    apply10 from10 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 12 elements in this order.
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
    -> Arr (In Nat12 (Nat12Plus a_)) element
from12 =
    apply11 from11 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 13 elements in this order.
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
    -> Arr (In Nat13 (Nat13Plus a_)) element
from13 =
    apply12 from12 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 14 elements in this order.
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
    -> Arr (In Nat14 (Nat14Plus a_)) element
from14 =
    apply13 from13 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 15 elements in this order.
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
    -> Arr (In Nat15 (Nat15Plus a_)) element
from15 =
    apply14 from14 (\init -> \last -> init |> inPush last)


{-| Create an `Arr` with exactly 16 elements in this order.
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
    -> Arr (In Nat16 (Nat16Plus a_)) element
from16 =
    apply15 from15 (\init -> \last -> init |> inPush last)



-- ## modify


{-| Set the element at an index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    Arr.from3 "I" "am" "ok"
        |> Arr.replaceAt nat2 FirstToLast "confusion"
    --> Arr.from3 "I" "am" "confusion"

    Arr.from3 "I" "am" "ok"
        |> Arr.replaceAt nat1 LastToFirst "feel"
    --> Arr.from3 "I" "feel" "ok"

-}
replaceAt :
    Nat (ArgIn indexMin_ minLengthMinus1 indexIfN_)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minLengthMinus1) max) element
    -> Arr (In (Nat1Plus minLengthMinus1) max) element
replaceAt index direction replacingElement =
    Internal.replaceAt index direction replacingElement


{-| Change the element at an index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/) based on its previous value.

    Arr.from3 1 20 30
        |> Arr.updateAt nat0 FirstToLast ((*) 10)
    --> Arr.from3 10 20 30

    Arr.from3 10 20 -30
        |> Arr.updateAt nat0 LastToFirst (\x -> -x)
    --> Arr.from3 10 20 30

-}
updateAt :
    Nat (ArgIn indexMin_ minLengthMinus1 indexIfN_)
    -> LinearDirection
    -> (element -> element)
    -> Arr (In (Nat1Plus minLengthMinus1) max) element
    -> Arr (In (Nat1Plus minLengthMinus1) max) element
updateAt index direction updateElement =
    \arr ->
        arr
            |> replaceAt index
                direction
                (updateElement (arr |> at index direction))


{-| Only keep values that satisfy a test.

    Arr.from5 1 2 3 4 5
        |> Arr.takeWhen (\n -> n >= 3)
    --> Arr.from3 3 4 5
    --> : Arr (In Nat0 (Nat5Plus a_)) number_

-}
takeWhen :
    (element -> Bool)
    -> Arr (In min max) element
    -> Arr (In Nat0 max) element
takeWhen isGood =
    Internal.takeWhen isGood


{-| Remove values that satisfy a test.

    Arr.from5 1 2 3 4 5
        |> Arr.dropWhen (\n -> n < 3)
    --> Arr.from3 3 4 5
    --> : Arr (In Nat0 (Nat5Plus a_)) number_

-}
dropWhen :
    (element -> Bool)
    -> Arr (In min max) element
    -> Arr (In Nat0 max) element
dropWhen isBad =
    takeWhen (not << isBad)



-- ## part


{-| A certain number of elements from a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    Arr.from8 0 1 2 3 4 5 6 7
        |> Arr.takeMax nat7 between3And7 FirstToLast
    --> : Arr (In Nat3 Nat7) ...

The first number is the maximum taken amount. The second number is the amount of taken elements.

Use [`take`](Arr#take) if you know the exact amount of elements to take.

-}
takeMax :
    Nat (N maxTaken atLeastMaxTaken (Is maxTakenToMin_ To min) is_)
    -> Nat (ArgIn minTaken maxTaken takenIfN_)
    -> LinearDirection
    -> Arr (In min max_) element
    -> Arr (In minTaken atLeastMaxTaken) element
takeMax maxAmount takenAmount direction =
    Internal.takeMax maxAmount takenAmount direction


{-| A certain number of elements from a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    Arr.from8 0 1 2 3 4 5 6 7
        |> Arr.take nat7 FirstToLast
    --> : Arr (In Nat3 Nat7) ...

Use [`takeMax`](Arr#takeMax) if you don't know the exact amount of elements to take.

-}
take :
    Nat (N taken atLeastTaken (Is takenToMin_ To min) is_)
    -> LinearDirection
    -> Arr (In min max_) element
    -> Arr (In taken atLeastTaken) element
take takenAmount direction =
    Internal.take takenAmount direction



-- ## transform


{-| Change every element.

    aToZ : Arr (In Nat26 (Nat26Plus a_)) Char
    aToZ =
        Arr.nats nat26
            |> Arr.map (val >> inABC)

    inABC =
        (+) ('a' |> Char.toCode)
            >> Char.fromCode

`val` refers to [`Typed.val`](https://package.elm-lang.org/packages/lue-bird/elm-typed-value/latest/Typed#val).

-}
map :
    (aElement -> mappedElement)
    -> Arr length aElement
    -> Arr length mappedElement
map alter =
    Internal.map alter


{-| Take every `Just value` & drop every `Nothing`.

    Arr.from3 (Just "This") Nothing (Just "fine")
        |> Arr.values
    --> Arr.from2 "This" "fine"
    --> : Arr (In Nat0 (Nat3Plus a_)) String

Often, calling `map` before `values` is helpful. Many call this combination "filterMap".

    Arr.from3 "1.2" "2" "hello"
        |> Arr.map String.toInt
        |> Arr.values
    --> Arr.from1 2
    --> : Arr (In Nat0 (Nat3Plus a_)) Int

-}
values :
    Arr (In min_ max) (Maybe value)
    -> Arr (In Nat0 max) value
values maybes =
    Internal.values maybes


{-| Combine the elements of 2 `Arr`s into new elements.
If one list is longer, its extra elements are not used.

    teamLifes aBoard bBoard =
        Arr.map2 (\a b -> a.lifes + b.lifes)
            aBoard
            bBoard

The length of all `Arr`s must be in the same range.

  - If 1 `Arr`'s maximum length isn't known:

```
Arr.map2 (\a b -> a.lifes + b.lifes)
    (aBoard |> MinArr.value)
    (bBoard |> MinArr.value)

aBoard : Arr (In Nat2 someMax) Field
bBoard : Arr (In Nat2 otherMax) Field
```

  - If 1 `Arr` has a higher minimum length:

```
Arr.map2 (\a b -> a.lifes + b.lifes)
    (aBoard |> Arr.lowerMinLength nat2)
    bBoard

aBoard : Arr (In Nat5 Nat10) Field
```

-}
map2 :
    (a -> b -> combinedElement)
    -> Arr length a
    -> Arr length b
    -> Arr length combinedElement
map2 combine aArr bArr =
    Internal.map2 combine aArr bArr


{-| Combine the elements of 3 `Arr`s into new elements. Works like [map2](Arr#map2).
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


{-| Combine the elements of 4 `Arr`s into new elements. Works like [map2](Arr#map2).
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
    -> Arr length_ element
    -> result
fold direction reduce initial =
    toArray >> Array.fold direction reduce initial


{-| A fold in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/) where the initial result is the first value in the `Arr`.

    Arr.foldWith FirstToLast maximum
        (Arr.from3 234 345 543)
    --> 543

    Arr.foldWith LastToFirst (++)
        (Arr.from3 "m" "l" "e")
    --> "elm"

Often, calling `map` before `foldWith` is helpful.

    Arr.map String.fromInt
        >> Arr.foldWith FirstToLast (++)

-}
foldWith :
    LinearDirection
    -> (element -> element -> element)
    -> Arr (In (Nat1Plus minMinus1_) max_) element
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


{-| Flip the order of the elements.

    Arr.from5 "l" "i" "v" "e"
        |> Arr.reverse
    --> Arr.from5 "e" "v" "i" "l"

-}
reverse : Arr length element -> Arr length element
reverse =
    Internal.reverse


{-| Resize an `Arr` in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/), padding with a given value.

    Arr.resize LastToFirst nat4 0
        (Arr.from2 1 2)
    --> Array.from4 0 0 1 2

    Arr.resize LastToFirst nat2 0
        (Array.from3 1 2 3)
    --> Array.from2 2 3

    Arr.resize FirstToLast nat4 0
        (Array.from2 1 2)
    --> Array.from4 1 2 0 0

    Arr.resize FirstToLast nat2 0
        (Array.from3 1 2 3)
    --> Array.from2 1 2

This is a quick way to gain some type-level knowledge about the length.

-}
resize :
    LinearDirection
    -> Nat (ArgIn newMin newMax ifN_)
    -> element
    -> Arr length_ element
    -> Arr (In newMin newMax) element
resize direction newLength paddingValue =
    Internal.resize direction newLength paddingValue



-- ## scan


{-| The amount of elements.

    Arr.length (Arr.from3 1 2 3)
    --> nat3 : Nat (In Nat3 (Nat3Plus a_))

    Arr.length between3And5Elements
    --> : Nat (In Nat3 (Nat5Plus a_))

    Arr.length atLeast3Elements
    --> : Nat (Min Nat3)

-}
length : Arr length element_ -> Nat length
length =
    Internal.length


{-| The element at a valid position in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    Arr.from4 1 2 3 4
        |> Arr.at nat1 FirstToLast
    --> 2

    Arr.from4 1 2 3 4
        |> Arr.at nat1 LastToFirst
    --> 3

-}
at :
    Nat (ArgIn indexMin_ minMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) max_) element
    -> element
at index direction =
    Internal.at index direction



-- ## extra


{-| Split the `Arr` into equal-sized chunks in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    { groups : the Arr divided into equal-sized Arrs
    , less : values to one side which aren't enough
    }

    Arr.from7 1 2 3 4 5 6 7
        |> Arr.groupsOf nat5 FirstToLast
    --> { groups =
    -->     Arr.from1 (Arr.from5 1 2 3 4 5)
    -->     : Arr (In Nat0 (Nat7Plus a_))
    -->         (Arr (In Nat5 (Nat5Plus b_)) number_)
    --> , remaining =
    -->     Arr.from2 6 7
    -->     : Arr (In Nat0 (Nat5Plus c_)) number_
    --> }

    Arr.from7 1 2 3 4 5 6 7
        |> Arr.groupsOf nat5 LastToFirst
    --> { groups = Arr.from1 (Arr.from5 3 4 5 6 7) : ...
    --> , remaining = Arr.from2 1 2 : ...
    --> }

-}
groupsOf :
    Nat (ArgIn (Nat1Plus minGroupSizeMinus1) maxGroupSize groupSizeIfN_)
    -> LinearDirection
    -> Arr (In min_ max) element
    ->
        { groups :
            Arr
                (In Nat0 max)
                (Arr
                    (In (Nat1Plus minGroupSizeMinus1) maxGroupSize)
                    element
                )
        , remaining : Arr (In Nat0 maxGroupSize) element
        }
groupsOf groupSize direction =
    Internal.groupsOf groupSize direction



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
    Nat (ArgIn lowerMin min lowerIfN_)
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
    Nat (ArgIn max newMax ifN_)
    -> Arr (In min max) element
    -> Arr (In min newMax) element
restoreMaxLength maximumLength =
    Internal.restoreMaxLength maximumLength



-- ## create


{-| Increasing natural numbers. In the end, there are `length` numbers.

    Arr.nats between2And10
    --> : Arr
    -->     (In Nat2 (Nat10Plus a))
    -->     (Nat (In Nat0 (Nat9Plus a)))

    from first length =
        Arr.nats length
            |> Arr.map (InNat.add first)

If you want to use a `Nat` as the length, but you dont know the maximum, e.g.

    Arr.nats (Nat.intAtLeast nat5 someInt) -- error

use [minNats](Arr#minNats).

-}
nats :
    Nat (ArgIn minLength (Nat1Plus maxLengthMinus1) lengthIfN_)
    ->
        Arr
            (In minLength (Nat1Plus maxLengthMinus1))
            (Nat (In Nat0 maxLengthMinus1))
nats length_ =
    Internal.nats length_


{-| Increasing natural numbers. In the end, there are `length` numbers. Use [nats](Arr#nats) if you know the maximum length.

    Arr.nats nat5

    Arr.nats between2And10

If not:

    Arr.minNats atLeast10
    --> : Arr (Min Nat10)
    -->     (Nat (Min Nat10))

    from first length =
        Arr.minNats length
            |> Arr.map (MinNat.add first)

-}
minNats :
    Nat (ArgIn minLength maxLength lengthIfN_)
    ->
        Arr
            (In minLength maxLength)
            (Nat (In Nat0 maxLength))
minNats length_ =
    Internal.minNats length_


{-| Generate a given amount of elements and put them in an `Arr`.

    Arr.random nat5 (Random.float 0 1)
    --> : Random.Generator (Arr (In Nat5 (Nat5Plus a_)) Float)

-}
random :
    Nat (ArgIn min max ifN_)
    -> Random.Generator element
    -> Random.Generator (Arr (In min max) element)
random amount generateElement =
    Internal.random amount generateElement


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize `Arr`s with a specific amount of elements.

    import Serialize


    -- we can't start if we have no worlds to choose from!
    serializeGameRow :
        Serialize.Codec
            String
            (Arr (Only Nat10) GameField)
    serializeGameRow =
        Arr.serialize nat10 serializeGameField

    encode : Arr (Only Nat10) GameField -> Bytes
    encode =
        Serialize.encodeToBytes serializeGameRow

    decode :
        Bytes
        ->
            Result
                (Serialize.Error String)
                (Arr (Only Nat10) GameField)
    decode =
        Serialize.decodeFromBytes serializeGameRow

-}
serialize :
    Nat (ArgIn min max ifN_)
    -> Serialize.Codec String element
    -> Serialize.Codec String (Arr (In min max) element)
serialize length_ serializeElement =
    Internal.serialize length_ serializeElement
