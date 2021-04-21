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

You get a _value, not a `Maybe`_.

Setup

```noformatingplease
elm install lue-bird/elm-bounded-nat
elm install lue-bird/elm-linear-direction
elm install lue-bird/elm-typesafe-array
```

If you want, take a look at both packages to get a feel why they are useful.
- [bounded-nat](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/)
- [linear-direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

```elm
import Nat exposing (Nat, Only, In, ValueIn, ValueMin)
import MinNat
import InNat
import NNat
import NNats exposing (..)
    --nat0 to nat160

import TypeNats exposing (..)
    --Nat0 to Nat160 & Nat0Plus to Nat160Plus

import LinearDirection exposing (LinearDirection(..))

import Arr exposing (Arr)
import NArr
import InArr
import MinArr
```

## How does that look like?

We will compare an `Arr` to an `Array`.

```elm
christmas =
    Arr.repeat nat3 "Ho"
        |> NArr.extend nat2 (Arr.from2 "it's" "christmas")

christmasArray =
    Array.append
        (Array.repeat 3 "Ho")
        (Array.fromList [ "it's", "christmas" ])

christmas |> Arr.at nat4 FirstToLast
--> "christmas"

christmasArray |> Array.get 4
--> Just "christmas"
```

_This_ is what this package is all about: Making these operations safe.
We _know_ there are 5 elements there.

→ You can define & use operations for `Arr`s with a certain amount

## specify a minimum length

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

### specify a maximum length
  
```elm
-- the max tag count should be 53
tag : Arr (In min Nat53 maybeN) Tag -> a -> Tagged a
tag tags toTag = --...

tag (Arr.from3 "fun" "easy" "fresh")
--> valid

tag (Arr.repeat nat100 EmptyTag)
--> compile-time error
```

## specify an exact length

```elm
type alias TicTacToeBoard rowsMaybeN columnsMaybeN =
    Arr (Only Nat3 rowsMaybeN)
        (Arr (Only Nat3 columnsMaybeN) TicTacToeField)

initialTicTacToeBoard : TicTacToeBoard --...
initialTicTacToeBoard =
    Arr.repeat nat3
        (Arr.repeat nat3 FieldEmpty)

type Field =
    FieldEmpty
    | X
    | O
```
