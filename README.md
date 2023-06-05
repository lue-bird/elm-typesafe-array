## [elm typesafe array](https://dark.elm.dmy.fr/packages/lue-bird/elm-typesafe-array/latest/)

Knowing more about the length of an `Array` at compile-time to help you **access elements safely**

```elm
ticTacToeBoard
    |> ArraySized.element ( Up, n2 )
    |> ArraySized.element ( Up, n1 )
```

**gives the element, no `Maybe`**
if `ticTacToeBoard`'s type proves it contains enough elements.
Such a type could be:

```elm
type alias TicTacToeBoard =
    -- 3 by 3
    ArraySized (ArraySized Field (Exactly (On N3))) (Exactly (On N3))

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
    |> ArraySized.element ( Up, n1 )
--→ Empty (indexes start with 1)
```
**We & the compiler knew** there were enough elements in `ticTacToeBoard`

## 🧩

  - 🔢 index, length : [`bounded-nat`][bounded-nat]
      - `n<x>`, `Min`, `In`, `Exactly`, `Up`, `Up<x>`, `On`, `N<x>`, `Add<x>`
      - ```bash
        elm install lue-bird/elm-bounded-nat
        ```
  - ↔️ which side to face : [`linear-direction`][linear-direction]
      - `Up | Down`
      - ```bash
        elm install lue-bird/elm-linear-direction
        ```

just a quick look will be fine. You can always come back to understand types etc. deeper

Let's define & use operations for all kinds of ranges ↓

## a minimum length?

```elm
import Linear exposing (Direction(..)) -- .. = Up, Down
import N exposing (In, On, Add1)
import ArraySized exposing (ArraySized)

last :
    ArraySized element (In (On (Add1 minFrom1_)) max_)
    -> element
last =
    ArraySized.element ( Down, n1 ) -- indexes start with 1

greatest :
    ArraySized comparable (In (On (Add1 minFrom1_)) max_)
    -> comparable
greatest =
    ArraySized.fold Up max

first ArraySized.empty -- compile-time error
greatest ArraySized.empty -- compile-time error
```

`ArraySized ... (In (On (Add1 minFrom1_)) max_)` means what exactly?
→ It constrains the length of possible `ArraySized`s:

length is `In` a range
  - the minimum length constraint is,
    without adding anything,
    on `1 + ` a variable (`1 + 0` | `1 + 1` | `1 + ...`)
    → `On (Add1 minFrom1_)`
  - any maximum length constraint is allowed
    (even [no maximum at all](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#Infinity))
    → `max_`

The types are explained in more detail in [`bounded-nat`][bounded-nat]

## an exact length?

Like in the tic-tac-toe example

```elm
import Linear exposing (Direction(..))
import N exposing (n2, n4, n7, n8, N8, Exactly, On)
import ArraySized exposing (ArraySized)

type alias ChessBoard =
    -- 8 by 8
    ArraySized (ArraySized Field (Exactly (On N8))) (Exactly (On N8))

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
        |> ArraySized.attach Up
            (ArraySized.repeat (ArraySized.repeat Empty n8) n4)
        |> ArraySized.push (pawnRow Black)
        |> ArraySized.push (firstRow Black)

initialChessBoard
    |> ArraySized.element ( Up, n2 )
    |> ArraySized.element ( Up, n7 )
--> Piece Pawn White
--  (indexes start with 1)
```


## a maximum length?

```elm
import N exposing (In, Up, To, N16)
import ArraySized exposing (ArraySized)

-- the max tag count should be 16
tag :
    ArraySized String (In min_ (Up maxTo16_ To N16))
    -> (Metadata -> MetadataTagged)
tag tags =
    ...

tag (ArraySized.l3 "fun" "easy" "simple") -- valid
tag (ArraySized.repeat "into-the-trends" n100) -- type error
```

## ready? go!

  - [`module ArraySized`](ArraySized) documents everything to start
  - [some example apps using `ArraySized`](https://github.com/lue-bird/elm-typesafe-array/tree/master/examples)
  - [elm-bits](https://package.elm-lang.org/packages/lue-bird/elm-bits/latest/): bits stored in an [`ArraySized`](ArraySized#ArraySized)
  - [elm-morph](https://package.elm-lang.org/packages/lue-bird/elm-morph/latest/) can safely parse a number of elements in a given range using [`ArraySized`](ArraySized#ArraySized)
  - [schach](https://github.com/lue-bird/schach) with a 8x8 [`ArraySized`](ArraySized#ArraySized) chess board

## [Orasund's `static-array`][static-array] – comparison

`typesafe-array` development started before `static-array` was published
but the ideas are similar

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
    for more then 16 elements,
    you can always easily safely
    [`attach`](ArraySized#attach) another [`ArraySized`](ArraySized#ArraySized)

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

    > Notice that we can NOT do addition in compile time, therefore we need to construct 6+6 manually

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
        ArraySized ... (In (Up6 minX_) (Up6 maxX_))

    arr1 |> ArraySized.attach Up arr2
    --: ArraySized ... (In (Up12 minX_) (Up12 maxX_))
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
        -> ArraySized element (In min (Up x To maxPlusX))
        -> ArraySized element (In min (Up x To (Add1 maxPlusX)))
    ```

### anything `static-array` is better at?
  - separating length and index types
  - simple, easy to understand types

[bounded-nat]: https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/
[static-array]: https://package.elm-lang.org/packages/Orasund/elm-static-array/latest/
[linear-direction]: https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/
