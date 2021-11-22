# elm-typesafe-array

Knowing more about the length of an `Array` at compile-time to help you **access elements safely**.

```elm
ticTacToeBoard
    |> Arr.at nat2 FirstToLast
    |> Arr.at nat0 FirstToLast
```

_returns a value, not a `Maybe`_ if `ticTacToeBoard`'s type can promise that it contains enough elements.

A type that can hold that promise could be:

```elm
type alias TicTacToeBoard =
    -- 3 by 3
    Arr (Only Nat3) (Arr (Only Nat3) Field)

type Field
    = Empty
    | X
    | O

aTicTacToeBoard : TicTacToeBoard
aTicTacToeBoard =
    Arr.from3
        (Arr.from3 Empty Empty O)
        (Arr.from3 Empty O Empty)
        (Arr.from3 O Empty Empty)
```

**We & the compiler know** there are enough elements in `aTicTacToeBoard`:

```elm
aTicTacToeBoard
    |> Arr.at nat2 FirstToLast
    |> Arr.at nat0 FirstToLast
--> O
```

## Setup

```noformattingples
elm install lue-bird/elm-linear-direction
elm install lue-bird/elm-typed-value
elm install lue-bird/elm-bounded-nat
elm install lue-bird/elm-typesafe-array
```

You can take a 👀 at these packages:
- [`bounded-nat`][bounded-nat]: `nat...`, `Nat...`, `Min`, `In`, `Only`
- a small extra: [`linear-direction`][linear-direction]: `FirstToLast` & `LastToFirst`

```elm
import LinearDirection exposing (LinearDirection(..))

import Nat exposing (Nat, Only, In, Min)
import Nats exposing (..)
    -- nat0-160, Nat0-160 & -Plus

import Typed

import Arr exposing (Arr)
import InArr
import MinArr
```

You can define & use operations for `Arr`s with a certain amount.

## a minimum length?

```elm
last :
    Arr (In (Nat1Plus orMore_) max_) element
    -> element
last =
    Arr.at nat0 LastToFirst

biggest :
    Arr (In (Nat1Plus orMore_) max_) comparable
    -> comparable
biggest =
    Arr.foldWith FirstToLast max

first Arr.empty --> compile-time error
biggest Arr.empty --> compile-time error
```

`Arr (In (Nat1Plus orMore_) max_)` means what exactly?
→ It constrains the length of possible `Arr`s.

The types are explained in more detail in [`bounded-nat`][bounded-nat] (only `In`, `Min` & `Only` is needed). In this example:

- `Array` with type info about its length: `Arr`
    - length is in a range: `In`
        - the minimum length is >= 1: `Nat1Plus orMore_`
        - no maximum length or any maximum length: `max_`

## an exact length?

Like in the tic-tac-toe example.

```elm
type alias ChessBoard =
    -- 8 by 8
    Arr (Only Nat8) (Arr (Only Nat8) Field)

type Field
    = Empty
    | Piece PieceKind Color

type PieceKind
    = Pawn
    --| ...

type Color
    = Black
    | White

initialChessBoard : ChessBoard
initialChessBoard =
    let
        pawnRow color =
            Arr.repeat nat8 (Piece Pawn color)

        firstRow color =
            Arr.repeat nat8 (Piece Other color)
    in
    Arr.empty
        |> InArr.push (firstRow White)
        |> InArr.push (pawnRow White)
        |> InArr.append nat4
            (Arr.repeat nat4 (Arr.repeat nat8 Empty))
        |> InArr.push (pawnRow Black)
        |> InArr.push (firstRow Black)

initialChessBoard
    |> Arr.at nat1 FirstToLast
    |> Arr.at nat6 FirstToLast
--> Piece Pawn White
```


## a maximum length?
  
```elm
-- the max tag count should be 53
tag : Arr (In min_ Nat53) String -> a -> Tagged a
tag tags toTag =
    ...

tag (Arr.from3 "fun" "easy" "fresh")
--> valid

tag (Arr.repeat nat100 "please-get-me-into-the-trends")
--> compile-time error
```

Now take a look at modules like `Arr` to get started!

## comparison to [Orasund's static-array][static-array]

Development of `typesafe-array` started before `static-array` was published but the idea is the same as from this package.

#### creating

`static-array`:
```elm
six =
    StaticArray.Length.five |> StaticArray.Length.plus1

StaticArray.fromList six 0 [ 1, 2, 3, 4, 5 ]
```
It makes it easy to forget the length if you add a new element to the list:

```elm
StaticArray.fromList six 0 [ 1, 2, 3, 4, 5, 6 ]
```

The added element `6` is simply ignored!

`typesafe-array`:
```elm
Arr.from6 0 1 2 3 4 5

Arr.from6 0 1 2 3 4 5 6
--> compile-time error
```

#### appending

`static-array`:
```elm
staticArray1, staticArray2 : StaticArray Six ...

let
    array1 =
        staticArray1 |> StaticArray.toRecord
    
    array2 =
        staticArray2 |> StaticArray.toRecord
in
StaticArray.fromRecord
    { length = StaticArray.Length.twelve
    , head = array1.head
    , tail = Array.append (array1.tail |> Array.push array2.head) array2.tail
    }
```
important note from static-array:

> Notice that we can NOT do addition in compile time, therefore we need to construct 6+6 manually.

→ You could easily do

```elm
wrong =
    StaticArray.fromRecord
        { length = StaticArray.Length.eight
        , head = array1.head
        , tail = Array.empty
        }
```

→ length in the type doesn't match the actual length

```elm
wrong
    |> StaticArray.get
        (StaticArray.Index.last StaticArray.Length.eight)
```

It silently gave us back an element at the wrong (first) index!

`typesafe-array`:
```elm
arr1, arr2 : Arr (In Nat6 (Nat6Plus a_)) ...

arr1 |> InArr.append nat6 arr2
--: Arr (In Nat12 (Nat12Plus a_)) ...
```

type-safe.

#### length in a range

`static-array`:
```elm
maybePush : Maybe a -> StaticArray n a -> ? --what result type?

type MaybePushResult lengthBefore
    = Pushed
        (StaticArray    
            (StaticArray.Index.OnePlus lengthBefore)
        )
    | DidntPush (StaticArray lengthBefore)

maybePush : Maybe a -> StaticArray n a -> MaybePushResult n
```
That's really inconvenient.

`typesafe-array`:
```elm
maybePush :
    Maybe a
    -> Arr (In min max) a
    -> Arr (In min (Nat1Plus max)) a
maybePush maybePushedElement =
    InArr.appendIn nat0 nat1
        (Arr.from1 maybePushedElement
            |> Arr.whenJust
        )
```

## ready? go!

- [some example apps that use `Arr`](https://github.com/lue-bird/elm-typesafe-array/tree/master/examples)
- [elm-bits](https://package.elm-lang.org/packages/lue-bird/elm-bits/latest/): bits stored in `Arr`

[bounded-nat]: https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/
[static-array]: https://package.elm-lang.org/packages/Orasund/elm-static-array/latest/
[linear-direction]: https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/
