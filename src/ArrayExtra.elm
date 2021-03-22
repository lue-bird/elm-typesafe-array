module ArrayExtra exposing (combine2, reverse)

import Array exposing (Array)


reverse : Array a -> Array a
reverse =
    Array.foldr Array.push Array.empty


combine2 : (a -> b -> c) -> Array a -> Array b -> Array c
combine2 combine aArray bArray =
    List.map2 combine
        (Array.toList aArray)
        (Array.toList bArray)
        |> Array.fromList
