module Candidates exposing (whenAllJustWithListCons, whenAllJustWithArrayPush)

import Array exposing (Array)
import Maybe

whenAllJustWithListCons : Array (Maybe a) -> Maybe (Array a)
whenAllJustWithListCons =
    Array.toList
        >> whenAllJustInList
        >> Maybe.map (Array.fromList)

whenAllJustInList =
    List.foldr (Maybe.map2 (::)) (Just [])

whenAllJustWithArrayPush : Array (Maybe a) -> Maybe (Array a)
whenAllJustWithArrayPush =
    Array.foldl (Maybe.map2 Array.push) (Just Array.empty)
