module ArrayExtra exposing (reverse)

import Array exposing (Array)


reverse : Array a -> Array a
reverse =
    Array.toList >> List.reverse >> Array.fromList
