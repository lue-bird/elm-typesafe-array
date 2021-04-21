module Extra.List exposing (groupsOf)

{-| -}


{-| Split a `List` into chunks of equal sizes (apart from maybe the last).

    groupsOf 3 (List.range 1 8)
    --> [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8 ] ]

-}
groupsOf : Int -> List a -> List (List a)
groupsOf size list =
    case list of
        [] ->
            []

        _ ->
            List.take size list
                :: (List.drop size list |> groupsOf size)
