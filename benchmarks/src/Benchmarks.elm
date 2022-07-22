module Benchmarks exposing (main)

import Array exposing (Array)
import Array.Extra as Array
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram)
import Candidates exposing (..)


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    describe "array"
        [ let
            maybes =
                ints1To100 |> Array.map Just
          in
          compare "allFill"
            (\f -> f maybes)
            ( "with List.cons"
            , areAllFilledWithListCons
            )
            ( "with Array.push"
            , areAllFilledWithArrayPush
            )
        ]


compare :
    String
    -> (a -> b_)
    -> ( String, a )
    -> ( String, a )
    -> Benchmark
compare name applyArguments ( aDescription, aFunction ) ( bDescription, bFunction ) =
    Benchmark.compare name
        aDescription
        (\() -> applyArguments aFunction)
        bDescription
        (\() -> applyArguments bFunction)


ints1To100 : Array Int
ints1To100 =
    Array.fromList (List.range 1 100)
