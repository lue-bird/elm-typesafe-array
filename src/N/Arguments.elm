module N.Arguments exposing (..)

{-| Helpers to work with functions.

This is often useful, for example when creating & extracting elements from a Vector.

Tell me if you find it useful and if I should publish this in its own package (together with other useful stuff (argument maps, folds, holes, drop, ...)).

-}


apply1 : (a -> b) -> (b -> c) -> a -> c
apply1 fun more =
    \a -> fun a |> more


apply2 : (a -> b -> c) -> (c -> d) -> a -> b -> d
apply2 fun more =
    \a -> apply1 (fun a) more


apply3 : (a -> b -> c -> d) -> (d -> e) -> a -> b -> c -> e
apply3 fun more =
    \a -> apply2 (fun a) more


apply4 : (a -> b -> c -> d -> e) -> (e -> f) -> a -> b -> c -> d -> f
apply4 fun more =
    \a -> apply3 (fun a) more
