module Benchmarks exposing (main)

import Array
import Array.Extra as Array
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "array-extra improvements"
        [ let
            aInts =
                Array.initialize 100 identity

            bInts =
                aInts |> Array.map (\i -> -i)
          in
          describe "map2 & apply"
            [ Benchmark.compare "map2"
                "with List.map2"
                (\() ->
                    List.map2 Tuple.pair
                        (aInts |> Array.toList)
                        (bInts |> Array.toList)
                        |> Array.fromList
                )
                "with Array.Extra.map2"
                (\() ->
                    Array.map2 Tuple.pair aInts bInts
                )
            , let
                intToTuples =
                    aInts |> Array.map Tuple.pair
              in
              Benchmark.compare "apply"
                "with List.map2"
                (\() ->
                    List.map2 (\f b -> f b)
                        (intToTuples |> Array.toList)
                        (bInts |> Array.toList)
                        |> Array.fromList
                )
                "with Array.Extra.apply"
                (\() ->
                    Array.apply intToTuples bInts
                )
            ]
        , let
            maybeInts =
                Array.initialize 100
                    (\x ->
                        if (x |> modBy 3) == 0 then
                            Nothing

                        else
                            Just x
                    )
          in
          Benchmark.compare "filterMap"
            "with List.filterMap"
            (\() ->
                maybeInts
                    |> Array.toList
                    |> List.filterMap identity
                    |> Array.fromList
            )
            "with Array.Extra.filterMap"
            (\() ->
                Array.filterMap identity maybeInts
            )
        , let
            zipped =
                Array.initialize 100 (\a -> ( a, a ))
          in
          Benchmark.compare "unzip"
            "with Array.map ( Tuple.first, Tuple.second )"
            (\() ->
                ( zipped |> Array.map Tuple.first
                , zipped |> Array.map Tuple.second
                )
            )
            "with Array.Extra.unzip"
            (\() -> zipped |> Array.unzip)
        , let
            ints =
                Array.initialize 100 identity
          in
          Benchmark.compare "reverse"
            "foldr"
            (\() ->
                ints
                    |> Array.foldr Array.push Array.empty
            )
            "List.reverse"
            (\() ->
                ints
                    |> Array.toList
                    |> List.reverse
                    |> Array.fromList
            )
        ]
