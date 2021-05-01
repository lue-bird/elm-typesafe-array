# elm-typesafe-array

Knowing more about the length of an `Array` at compile-time to help you **access elements safely**.

```elm
chessBoard
    |> Arr.at nat1 FirstToLast
    |> Arr.at nat6 FirstToLast
```

returns a **_value, not a `Maybe`_** if `chessBoard`'s type can promise that it contains enough elements.

```elm
initialChessBoard =
    let
        pawnRow color =
            Arr.repeat nat8 (Piece Pawn color)

        firstRow color =
            Arr.from8 --...
    in
    Arr.empty
        |> NArr.push (firstRow White)
        |> NArr.push (pawnRow White)
        |> NArr.extend nat4
            (Arr.repeat nat4
                (Arr.repeat nat8 Empty)
            )
        |> NArr.push (pawnRow Black)
        |> NArr.push (firstRow Black)

type Field
    = Empty
    | Piece PieceKind Color

type PieceKind = Pawn | --...
type Color = Black | White
```

**We know** there are enough elements in `initialChessBoard`. You can then easily ask:

```elm
initialChessBoard
    |> Arr.at nat1 FirstToLast
    |> Arr.at nat6 FirstToLast
--> Piece Pawn White
```

### Setup

If you want, take a 👀 to get a feel why the used packages are useful.
- most importantly: [bounded-nat][bounded-nat]: the `nat...` part, also for building the type
- [linear-direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/): `FirstToLast` & `LastToFirst`

```noformatingplease
elm install lue-bird/elm-linear-direction
elm install lue-bird/elm-typed-value
elm install lue-bird/elm-bounded-nat
elm install lue-bird/elm-typesafe-array
```

```elm
import LinearDirection exposing (LinearDirection(..))

import Nat exposing (Nat, Only, In, ValueIn, ValueMin)
import MinNat
import InNat
import NNats exposing (..)
    -- (..) is nat0 to nat160

import Typed exposing (val, val2)

import TypeNats exposing (..)
    -- (..) is Nat0 to Nat160 & Nat1Plus to Nat160Plus

import Arr exposing (Arr)
import NArr
import InArr
import MinArr
```

You can define & use operations for `Arr`s with a certain amount.

## a minimum length?

```elm
first :
    Arr (In (Nat1Plus orLonger) max maybeN) element
    -> element
first =
    Arr.at nat0 FirstToLast

biggest :
    Arr (In (Nat1Plus orLonger) max maybeN) comparable
    -> comparable
biggest =
    Arr.foldWith FirstToLast max

first Arr.empty --> compile-time error
biggest Arr.empty --> compile-time error
```

`Arr (In (Nat1Plus orLonger) max maybeN)` means what exactly?
→ It constrains the length of possible `Arr`s.

The types are explained in more detail in [`bounded-nat`][bounded-nat]. In this example:

- `Arr`: `Array` with additional type info about its length
    - `In`: length is within a minimum (& maximum)
        - `Nat1Plus orLonger`: the minimum length is >= 1
        - `max`: no maximum length or any maximum length
        - `maybeN` potential extra information if the length is exact

## an exact length?

```elm
type alias TicTacToeBoard =
    Arr (ValueOnly Nat3)
        (Arr (ValueOnly Nat3) TicTacToeField)

type TicTacToeField =
    FieldEmpty
    | X
    | O

initialTicTacToeBoard : TicTacToeBoard
initialTicTacToeBoard =
    Arr.repeat nat3
        (Arr.repeat nat3 FieldEmpty |> InArr.value)
        |> InArr.value
```

Why the `InArr.value` calls? It's because in situations like these, the type is more precise than a `ValueOnly`: It's a `ValueN` → you have to _explicitly drop type information_.


## a maximum length?
  
```elm
-- the max tag count should be 53
tag : Arr (In min Nat53 maybeN) Tag -> a -> Tagged a
tag tags toTag = --...

tag (Arr.from3 "fun" "easy" "fresh")
--> valid

tag (Arr.repeat nat100 EmptyTag)
--> compile-time error
```

Now take a look at modules like `Arr` to get started!

[bounded-nat]: https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/
