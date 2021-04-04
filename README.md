# elm-bounded-array

A `Arr length` is like an `Array`, but you know more about the amount of elements at compile-time.

```elm
startChessBoard =
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

type Color
    = Black
    | White

type Field
    = Empty
    | Piece PieceKind Color

type PieceKind
    = Pawn
    | --...
```

You can then easily ask:

```elm
startChessBoard
    |> InArr.at nat1 FirstToLast
    |> InArr.at nat6 FirstToLast
--> Piece Pawn White
```

You get a _value, not a `Maybe`_.

Setup

```noformatingplease
elm install indique/elm-bounded-nat
elm install indique/elm-linear-direction
elm install indique/elm-bounded-array
```

If you want, take a look at both packages to get a feel why they are useful.
- [bounded-nat](https://package.elm-lang.org/packages/indique/elm-bounded-nat/latest/)
- [linear-direction](https://package.elm-lang.org/packages/indique/elm-linear-direction/latest/)

```elm
import Nat exposing (Nat)
import Nat.Bound exposing (..)
    --In, Only, N, Is, And, To, ValueMin, ValueIn, ValueOnly
import MinNat
import InNat
import NNat
import NNats exposing (..) --nat0 to nat168

import Nat.Type exposing (..)
    --Nat0 to Nat192 & Nat0Plus to Nat192Plus

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
        |> NArr.extendN nat2 (Arr.from2 "it's" "christmas")

christmasArray =
    Array.repeat 3 "Ho"
        |> Array.append (Array.fromList [ "it's", "christmas" ])

christmas
    |> InArr.at nat4 FirstToLast
--> "christmas"

christmasArray
    |> Array.get 4
--> Just "christmas"
```

_This_ is what this package is all about. Making these operations safe.
We _know_ there are `Nat5` elements there.

```elm
shout christmas
--> HO HO HO IT'S CHRISTMAS!

shout words =
    words
        |> Arr.map String.toUpper
        --map takes an Arr of any size
        |> Arr.foldWith FirstToLast (\soFar word -> soFar ++ " " + word)
        --foldWithFirst takes an Arr with at least 1 element
        ++ "!"
```

And as you see, if we know, that it has at least `Nat5` elements, we also know that it has more than 1 element.

- You can define & use operations for `Arr`s with a certain amount
    ```elm
    first : Arr (In (Nat1Plus more) max maybeExact) element -> element
    first =
        MinArr.at nat0 FirstToLast

    biggestInRow :
        Arr (In (Nat1Plus minMinus1) max maybeExact) comparable
        -> comparable
    biggestInRow =
        Arr.foldWith FirstToLast max
    ```

### set a maximum length
  
    ```elm
    --the max tag count should be 53
    tag : Arr (In min Nat53 maybeExact) Tag -> a -> Tagged a
    tag tags toTag = --...

    tag
        ([ ( nat0 |> NNat.toIn, "fun" ), ( nat1, "easy" ), ( nat2, "fresh" ) ]
            |> List.map2 
            |> List.foldr
                (\( i, tag )-> Arr.replaceAt (i (Tag tag))
                (Arr.repeat nat53 EmptyTag)
        )
    ```

