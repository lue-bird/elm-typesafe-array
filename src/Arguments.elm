module Arguments exposing (apply1, apply10, apply11, apply12, apply13, apply14, apply15, apply16, apply2, apply3, apply4, apply5, apply6, apply7, apply8, apply9)


apply1 : (a -> b) -> (b -> c) -> a -> c
apply1 f more =
    \a -> f a |> more


apply2 : (a -> b -> c) -> (c -> d) -> a -> b -> d
apply2 f more =
    \a -> apply1 (f a) more


apply3 : (a -> b -> c -> d) -> (d -> e) -> a -> b -> c -> e
apply3 f more =
    \a -> apply2 (f a) more


apply4 : (a -> b -> c -> d -> e) -> (e -> f) -> a -> b -> c -> d -> f
apply4 f more =
    \a -> apply3 (f a) more


apply5 : (a -> b -> c -> d -> e -> f) -> (f -> g) -> a -> b -> c -> d -> e -> g
apply5 f more =
    \a -> apply4 (f a) more


apply6 : (a -> b -> c -> d -> e -> f -> g) -> (g -> h) -> a -> b -> c -> d -> e -> f -> h
apply6 f more =
    \a -> apply5 (f a) more


apply7 : (a -> b -> c -> d -> e -> f -> g -> h) -> (h -> i) -> a -> b -> c -> d -> e -> f -> g -> i
apply7 f more =
    \a -> apply6 (f a) more


apply8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> (i -> j) -> a -> b -> c -> d -> e -> f -> g -> h -> j
apply8 f more =
    \a -> apply7 (f a) more


apply9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> (j -> k) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> k
apply9 f more =
    \a -> apply8 (f a) more


apply10 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> (k -> l) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> l
apply10 f more =
    \a -> apply9 (f a) more


apply11 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l) -> (l -> m) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> m
apply11 f more =
    \a -> apply10 (f a) more


apply12 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m) -> (m -> n) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> n
apply12 f more =
    \a -> apply11 (f a) more


apply13 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n) -> (n -> o) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> o
apply13 f more =
    \a -> apply12 (f a) more


apply14 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o) -> (o -> p) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> p
apply14 f more =
    \a -> apply13 (f a) more


apply15 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p) -> (p -> q) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> q
apply15 f more =
    \a -> apply14 (f a) more


apply16 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> q) -> (q -> r) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> r
apply16 f more =
    \a -> apply15 (f a) more
