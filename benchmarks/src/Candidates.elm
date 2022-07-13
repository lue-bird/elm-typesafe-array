module Candidates exposing (areAllFilledWithListCons, areAllFilledWithArrayPush)

import Array exposing (Array)
import Maybe

areAllFilledWithListCons : Array (Maybe a) -> Maybe (Array a)
areAllFilledWithListCons =
    Array.toList
        >> areAllFilledInList
        >> Maybe.map (Array.fromList)

areAllFilledInList =
    List.foldr (Maybe.map2 (::)) (Just [])

areAllFilledWithArrayPush : Array (Maybe a) -> Maybe (Array a)
areAllFilledWithArrayPush =
    Array.foldl (Maybe.map2 Array.push) (Just Array.empty)
