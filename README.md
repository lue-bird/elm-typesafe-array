# elm-typesafe-array

Knowing more about the length of an `Array` at compile-time to help you **access elements safely**.

```elm
ticTacToeBoard
    |> Arr.at nat2 FirstToLast
    |> Arr.at nat1 FirstToLast
```

returns a **_value, not a `Maybe`_** if `ticTacToeBoard`'s type can promise that it contains enough elements.

A type that can hold that promise could be:

```elm
{-| 3 by 3 -}
type alias TicTacToeBoard =
    Arr (Only Nat3)
        (Arr (Only Nat3) TicTacToeField)

type TicTacToeField =
    FieldEmpty
    | X
    | O

initialTicTacToeBoard : TicTacToeBoard
initialTicTacToeBoard =
    Arr.repeat nat3
        (Arr.repeat nat3 FieldEmpty)
```

**We & the compiler know** there are enough elements in `initialTicTacToeBoard`:

```elm
initialTicTacToeBoard
    |> Arr.at nat2 FirstToLast
    |> Arr.at nat1 FirstToLast
--> FieldEmpty
```

### Setup

If you want, take a 👀 to get a feel why the used packages are useful.
- most importantly: [bounded-nat][bounded-nat]: `nat...`, `Nat...`, `Min`, `In`, `Only`
- [linear-direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/): `FirstToLast` & `LastToFirst`

```noformatingplease
elm install lue-bird/elm-linear-direction
elm install lue-bird/elm-typed-value
elm install lue-bird/elm-bounded-nat
elm install lue-bird/elm-typesafe-array
```

```elm
import LinearDirection exposing (LinearDirection(..))

import Nat exposing (Nat, Only, In, Min)
import InNat
import NNats exposing (..)
    -- (..) is nat0 to nat160

import Typed

import TypeNats exposing (..)
    -- (..) is Nat0 to Nat160 & Nat1Plus to Nat160Plus

import Arr exposing (Arr)
import InArr
import MinArr
```

You can define & use operations for `Arr`s with a certain amount.

## a minimum length?

```elm
first :
    Arr (In (Nat1Plus orLonger) max) element
    -> element
first =
    Arr.at nat0 FirstToLast

biggest :
    Arr (In (Nat1Plus orLonger) max) comparable
    -> comparable
biggest =
    Arr.foldWith FirstToLast max

first Arr.empty --> compile-time error
biggest Arr.empty --> compile-time error
```

`Arr (In (Nat1Plus orLonger) max)` means what exactly?
→ It constrains the length of possible `Arr`s.

The types are explained in more detail in [`bounded-nat`][bounded-nat] (only `In`, `Min` & `Only` is used for `Arr`s). In this example:

- `Arr`: `Array` with additional type info about its length
    - `In`: length is within a minimum (& maximum)
        - `Nat1Plus orLonger`: the minimum length is >= 1
        - `max`: no maximum length or any maximum length

## an exact length?

As we've seen in the tic-tac-toe example.

```elm
{-| 8 by 8 -}
type alias ChessBoard =
    Arr (Only Nat8) (Arr (Only Nat8) Field)

type Field
    = Empty
    | Piece PieceKind Color

type PieceKind = Pawn | --...
type Color = Black | White

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
        |> InArr.extend nat4
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
tag : Arr (In min Nat53) Tag -> a -> Tagged a
tag tags toTag = --...

tag (Arr.from3 "fun" "easy" "fresh")
--> valid

tag (Arr.repeat nat100 EmptyTag)
--> compile-time error
```

Now take a look at modules like `Arr` to get started!

## comparison to [Orasund's static-array](https://package.elm-lang.org/packages/Orasund/elm-static-array/latest/)

I started creating my package before this one so I didn't take inspiration from this package.

```elm
six = StaticArray.Length.five |> StaticArray.Length.plus1

StaticArray.fromList six 0[1,2,3,4,5]
```
vs
```elm
Arr.from6 0 1 2 3 4 5
```

```elm
-- array1, array2 from calling `StaticArray.toRecord` an a StaticArray Six

-- append them
StaticArray.fromRecord
    { length = StaticArray.Length.twelve
    , head = array1.head
    , tail = Array.append (array1.tail |> Array.push array2.head) array2.tail
    }
```
Note from static-array:

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
wrong |> StaticArray.get (StaticArray.Index.last StaticArray.Length.eight)
```

[`get`](https://package.elm-lang.org/packages/Orasund/elm-static-array/latest/StaticArray#get)'s documentation states:

It silently gave us back an element at the wrong (first) index!

vs

```elm
arr1, arr2 : Arr (Only Nat6) -- ...

arr1 |> InArr.extend nat6 arr2
-- : Arr (Only Nat12) ...
```

type-safe.

```elm
maybePush : Maybe a -> StaticArray n a -> -- what is the result?!
```
vs
```elm
maybePush :
    Maybe a
    -> Arr (In min max) a
    -> Arr (In min (Nat1Plus max)) a
```

[bounded-nat]: https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/
