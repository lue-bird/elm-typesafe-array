# elm-typesafe-array

An `Arr length` is like an `Array`, but you know more about the amount of elements at compile-time which can help you access elements safely.

```elm
initialChessBoard =
    let
        emptyRow =
            Arr.repeat nat8 Empty

        pawnRow color =
            Arr.repeat nat8 (Piece Pawn color)

        firstRow color =
            Arr.from8 --...
    in
    Arr.empty
        |> NArr.push (firstRow White)
        |> NArr.push (pawnRow White)
        |> NArr.extend nat4 (Arr.repeat nat4 emptyRow)
        |> NArr.push (pawnRow Black)
        |> NArr.push (firstRow Black)

type Field
    = Empty
    | Piece PieceKind Color

type Color
    = Black
    | White

type PieceKind
    = Pawn
    | --...
```

You can then easily ask:

```elm
initialChessBoard
    |> Arr.at nat1 FirstToLast
    |> Arr.at nat6 FirstToLast
--> Piece Pawn White
```

You get a _value, not a `Maybe`_. _This_ package is all about making these operations safe.
**We know** there are enough elements.

Setup

```noformatingplease
elm install lue-bird/elm-typesafe-array
elm install lue-bird/elm-bounded-nat
elm install lue-bird/elm-linear-direction
elm install lue-bird/elm-typed-value
```

If you want, take a 👀 to get a feel why they are useful.
- most importantly: [bounded-nat](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/)
    - which uses [typed-value](https://package.elm-lang.org/packages/lue-bird/typed-value/latest/)
- [linear-direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

```elm
import Nat exposing (Nat, Only, In, ValueIn, ValueMin)
import MinNat
import InNat
import NNat
import NNats exposing (..)
    -- nat0 to nat160

import TypeNats exposing (..)
    -- Nat0 to Nat160 & Nat0Plus to Nat160Plus

import Arr exposing (Arr)
import NArr
import InArr
import MinArr

import LinearDirection exposing (LinearDirection(..))

import Typed exposing (val, val2)
```

You can define & use operations for `Arr`s with a certain amount.

## a minimum length?

```elm
first : Arr (In (Nat1Plus orLonger) max maybeN) element -> element
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
        (Arr.repeat nat3 FieldEmpty |> NArr.toIn)
        |> NArr.toIn
```

Why the `NArr.toIn`? It's because in situations like these, the type is more precise than a `ValueOnly`: It's a `ValueN`. So you have to _explicitly drop type information_.


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
