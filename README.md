## [elm typesafe array](https://dark.elm.dmy.fr/packages/lue-bird/elm-typesafe-array/latest/)

Knowing more about the length of an `Array` at compile-time to help you **access elements safely**

```elm
ticTacToeBoard
    |> ArraySized.element ( Up, n2 )
    |> ArraySized.element ( Up, n0 )
```

_returns a value, not a `Maybe`_
if `ticTacToeBoard`'s type can promise that it contains enough elements.
Such a type could be:

```elm
type alias TicTacToeBoard =
    -- 3 by 3
    ArraySized (Exactly N3) (ArraySized (Exactly N3) Field)

type Field
    = Empty
    | X
    | O

ticTacToeBoard : TicTacToeBoard
ticTacToeBoard =
    ArraySized.l3
        (ArraySized.l3 Empty Empty O)
        (ArraySized.l3 Empty O Empty)
        (ArraySized.l3 O Empty Empty)

ticTacToeBoard
    |> ArraySized.element ( Up, n2 )
    |> ArraySized.element ( Up, n0 )
--→ O
```
**We & the compiler knew** there were enough elements in `ticTacToeBoard`

## building blocks

```noformattingples
elm install lue-bird/elm-linear-direction
elm install lue-bird/elm-bounded-nat
elm install lue-bird/elm-typesafe-array
```

  - numbers – [`bounded-nat`][bounded-nat]
      - `n<x>`, `N<x>`, `Add<x>`, `Min`, `In`, `Exactly`, `Up`, `Fixed`
  - from which side to look – [`linear-direction`][linear-direction]
      - `Up`, `Down`

```elm
import Linear exposing (DirectionLinear(..)) -- Up or Down
import N exposing (N, Exactly, Min, In, Up, To, Fixed)
import ArraySized exposing (ArraySized)
```

Let's define & use operations for `ArraySized`s with a certain amount of elements ↓

## a minimum length?

```elm
last :
    ArraySized (In (Fixed (Add1 minMinus1_)) max_) element
    -> element
last =
    ArraySized.element ( Down, n0 )

greatest :
    ArraySized (In (Fixed (Add1 minMinus1_)) max_) comparable
    -> comparable
greatest =
    ArraySized.fold Up max

first ArraySized.empty -- compile-time error
greatest ArraySized.empty -- compile-time error
```

`ArraySized (In (Fixed (Add1 orMore_)) max_)` means what exactly?
→ It constrains the length of possible `ArraySized`s.

The types are explained in more detail in [`bounded-nat`][bounded-nat] (`In`, `Min`, `Exactly`). In this example:

length is `In` a range
  - the minimum length constraint is,
    without adding anything,
    `1 + ` a variable (`1 + 0` | `1 + 1` | `1 + ...`)
    → `Fixed (Add1 minMinus1_)`
  - any maximum length constraint is allowed
    (even [none at all](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#MaxNo))
    → `max_`

## an exact length?

Like in the tic-tac-toe example

```elm
import Linear exposing (DirectionLinear(..))
import N exposing (n1, n4, n6, n8, N8, Exactly)
import ArraySized exposing (ArraySized)

type alias ChessBoard =
    -- 8 by 8
    ArraySized (Exactly N8) (ArraySized (Exactly N8) Field)

type Field
    = Empty
    | Piece PieceKind Color

type PieceKind
    = Pawn
    | Other --...

type Color
    = Black
    | White

initialChessBoard : ChessBoard
initialChessBoard =
    let
        pawnRow color =
            ArraySized.repeat (Piece Pawn color) n8
        firstRow color =
            ArraySized.repeat (Piece Other color) n8
    in
    ArraySized.empty
        |> ArraySized.push (firstRow White)
        |> ArraySized.push (pawnRow White)
        |> ArraySized.glue Up
            (ArraySized.repeat (ArraySized.repeat Empty n8) n4)
        |> ArraySized.push (pawnRow Black)
        |> ArraySized.push (firstRow Black)

initialChessBoard
    |> ArraySized.element ( Up, n1 )
    |> ArraySized.element ( Up, n6 )
--> Piece Pawn White
```


## a maximum length?
  
```elm
-- the max tag count should be 53
tag : ArraySized (In min_ N53) String -> (a -> Tagged a)
tag tags toTag =
    ...

tag (ArraySized.l3 "fun" "easy" "simple") -- valid
tag (ArraySized.repeat n100 "into-the-trends") -- type error
```

## ready? go!

  - [`module ArraySized`](ArraySized) documents everything to start
  - [some example apps using `ArraySized`](https://github.com/lue-bird/elm-typesafe-array/tree/master/examples)
  - [elm-bits](https://package.elm-lang.org/packages/lue-bird/elm-bits/latest/): bits stored in [`ArraySized`](ArraySized#ArraySized)

[bounded-nat]: https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/
[static-array]: https://package.elm-lang.org/packages/Orasund/elm-static-array/latest/
[linear-direction]: https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/


## [Orasund's `static-array`][static-array] – comparison

`typesafe-array` development started before `static-array` was published
but the ideas are really similar.

### create

  - `static-array`
    ```elm
    eleven =
        StaticArray.Length.ten |> StaticArray.Length.plus1

    StaticArray.fromList eleven 0 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
    ```
    makes it easy to forget the length if you add a new element or remove one
    ```elm
    StaticArray.fromList eleven 0 [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]
    ```
    the added element `11` is simply ignored!

  - `typesafe-array`
    ```elm
    ArraySized.l11 0 1 2 3 4 5 6 7 8 9 10

    ArraySized.l11 0 1 2 3 4 5 6 7 8 9 10 11 -- type error
    ```

### append

  - `static-array`
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
    important note from `static-array`:

    > Notice that we can NOT do addition in compile time, therefore we need to construct 6+6 manually.

    → You can easily do
    ```elm
    StaticArray.fromRecord
        { length = StaticArray.Length.eight
        , head = array1.head
        , tail = Array.empty
        }
        |> StaticArray.get
            (StaticArray.Length.eight |> StaticArray.Index.last)
    --→ array1.head
    ```
    The supplied length type doesn't match its actual length
    → we silently got back an element at the wrong (first) index!

  - `typesafe-array`
    ```elm
    arr1, arr2 :
        ArraySized (In (Up minX To (Add6 minX)) (Up maxX To (Add6 maxX))) ...

    arr1 |> ArraySized.glue Up arr2
    --: ArraySized (In (Up minX To (Add12 minX)) (Up maxX To (Add12 maxX))) ...
    ```
    type-safe

### length in a range

  - `static-array`
    ```elm
    maybePush :
        Maybe element
        -> StaticArray length element
        -> ? -- what result type?

    type MaybePushResult lengthBefore element
        = Pushed
            (StaticArray    
                (StaticArray.Index.OnePlus lengthBefore)
                element
            )
        | DidntPush (StaticArray lengthBefore element)

    maybePush :
        Maybe element
        -> StaticArray length element
        -> MaybePushResult length element
    ```
    really inconvenient

  - `typesafe-array`
    ```elm
    pushMaybe :
        Maybe element
        -> ArraySized (In min (Up x To maxPlusX)) element
        -> ArraySized (In min (Up x To (Add1 maxPlusX))) element
    ```

### anything `static-array` is better at?
  - separating length and index types
  - simple, easy to understand types
