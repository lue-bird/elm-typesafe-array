module Internal.Arr exposing (Arr(..), at, combine2, empty, extend, fromArray, insertAt, length, map, mapArrayAndLength, mapLength, nPush, push, random, range, removeAt, repeat, replaceAt, reverse, take, takeIn, takeN, toArray)

{-| Only use it in `Internal.Arr. ...` modules.
-}

import Array exposing (Array)
import Array.Extra
import ArrayExtra as Array
import InNat
import LinearDirection exposing (LinearDirection)
import LinearDirection.Array as Array
import MinNat
import NNat
import NNats exposing (..)
import Nat exposing (Nat)
import Nat.Bound exposing (..)
import Random
import TypeNats exposing (..)


type Arr length element
    = Arr (Array element) { length : Nat length }



-- ## scan


at :
    Nat (In indexMin minMinus1 indexMaybeN)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) max maybeN) element
    -> element
at index direction =
    \arr ->
        case Array.at (Nat.toInt index) direction (toArray arr) of
            Just element ->
                --succeeds for every correct typed Arr (should be)
                element

            Nothing ->
                --if not, we crash with a
                --RangeError: Maximum call stack size exceeded
                at index direction arr


length : Arr length element -> Nat length
length =
    \(Arr _ l) -> .length l



-- ## transform


toArray : Arr length element -> Array element
toArray =
    \(Arr array _) -> array


mapArrayAndLength :
    (Array element -> Array mappedElement)
    -> (Nat length -> Nat mappedLength)
    -> Arr length element
    -> Arr mappedLength mappedElement
mapArrayAndLength mapArray mapLen =
    \inArray ->
        Arr (mapArray (toArray inArray))
            { length = mapLen (length inArray) }


mapLength :
    (Nat length -> Nat mappedLength)
    -> Arr length element
    -> Arr mappedLength element
mapLength =
    mapArrayAndLength identity


map :
    (aElement -> bElement)
    -> Arr length aElement
    -> Arr length bElement
map alter =
    mapArrayAndLength (Array.map alter) identity


combine2 :
    (a -> b -> combined)
    -> Arr length a
    -> Arr length b
    -> Arr length combined
combine2 combine aArr bArr =
    Arr
        (Array.Extra.map2 combine
            (toArray aArr)
            (toArray bArr)
        )
        { length =
            Nat.theSmaller (length aArr) (length bArr)
        }



-- ## create


empty : Arr (N Nat0 (Is a To) a (And b To b)) element
empty =
    Arr Array.empty { length = nat0 }


repeat : Nat amount -> element -> Arr amount element
repeat amount element =
    Arr (Array.repeat (Nat.toInt amount) element)
        { length = amount }


fromArray : Array element -> Arr (ValueMin Nat0) element
fromArray =
    \array ->
        Arr array
            { length =
                Array.length array
                    |> Nat.intAtLeast (nat0 |> InNat.toMin)
            }


range :
    Nat (In minFirst maxFirst firstMaybeN)
    -> Nat (N maxFirst (Is maxFirstToMinLast To) minLast y)
    -> Nat (In minLast maxLast lastMaybeN)
    ->
        Arr
            (ValueMin (Nat1Plus maxFirstToMinLast))
            (Nat (ValueMin minFirst))
range first firstMax last =
    Arr
        (Nat.bi List.range first last
            |> List.map (Nat.intAtLeast (first |> InNat.toMin))
            |> Array.fromList
        )
        { length =
            last
                |> MinNat.sub first firstMax
                |> MinNat.addN nat1
        }


random :
    Nat length
    -> Random.Generator element
    -> Random.Generator (Arr length element)
random amount generateElement =
    Random.list (Nat.toInt amount) generateElement
        |> Random.map
            (\list ->
                Arr (Array.fromList list) { length = amount }
            )



-- ## modify


nPush :
    element
    -> Arr (N n (Is a To) nPlusA (And b To nPlusB)) element
    ->
        Arr
            (N
                (Nat1Plus n)
                (Is a To)
                (Nat1Plus nPlusA)
                (And b To (Nat1Plus nPlusB))
            )
            element
nPush elementToPush =
    push elementToPush (NNat.add ( nat1, nat1 ))


replaceAt :
    Nat (In indexMin minLengthMinus1 indexMaybeN)
    -> LinearDirection
    -> element
    -> Arr (In (Nat1Plus minLengthMinus1) max maybeN) element
    -> Arr (In (Nat1Plus minLengthMinus1) max maybeN) element
replaceAt index direction replacingElement =
    mapArrayAndLength
        (Array.replaceAt (index |> Nat.toInt) direction replacingElement)
        identity



-- ## part


takeIn :
    Nat (In minTaken maxTaken takenMaybeN)
    -> Nat (N maxTaken (Is maxTakenToMin To) min x)
    -> LinearDirection
    -> Arr (In min max maybeN) element
    -> Arr (In minTaken maxTaken takenMaybeN) element
takeIn amount maxAmount direction =
    take amount direction


takeN :
    ( Nat (N taken (Is dropped To) min x)
    , Nat (N taken isATo takenPlusA bDiff)
    )
    -> LinearDirection
    -> Arr (In min max maybeN) element
    -> Arr (N taken isATo takenPlusA bDiff) element
takeN amount direction =
    take (amount |> Tuple.second) direction


take :
    Nat amount
    -> LinearDirection
    -> Arr length element
    -> Arr amount element
take amount direction =
    mapArrayAndLength
        (Array.take (amount |> Nat.toInt) direction)
        (\_ -> amount)



-- ## extra


reverse : Arr length element -> Arr length element
reverse =
    mapArrayAndLength Array.reverse identity



-- presets


extend :
    Arr addedLength element
    -> (Nat addedLength -> Nat length -> Nat lengthSum)
    -> Arr length element
    -> Arr lengthSum element
extend extension addLength =
    mapArrayAndLength
        (\array -> Array.append array (toArray extension))
        (addLength (length extension))


removeAt :
    Nat index
    -> LinearDirection
    -> (Nat length -> Nat lengthMinus1)
    -> Arr length element
    -> Arr lengthMinus1 element
removeAt index direction sub1 =
    mapArrayAndLength
        (Array.removeAt (Nat.toInt index) direction)
        sub1


insertAt :
    Nat range
    -> LinearDirection
    -> element
    -> (Nat length -> Nat lengthPlus1)
    -> Arr length element
    -> Arr lengthPlus1 element
insertAt index direction inserted add1 =
    mapArrayAndLength
        (Array.insertAt (index |> Nat.toInt) direction inserted)
        add1


push :
    element
    -> (Nat length -> Nat lengthPlus1)
    -> Arr length element
    -> Arr lengthPlus1 element
push elementToPush add1 =
    mapArrayAndLength (Array.push elementToPush) add1
