module AboutUnionTypes exposing (testSuite)

import Expect
import Test exposing (describe, test)
import TestHelpers exposing (..)


type Nucleotide
    = A
    | C
    | G
    | T


xNucleotide =
    G


type DNA
    = Base Nucleotide
    | Strand (List Nucleotide)


testSuite =
    describe "About UnionTypes"
        [ test "simple types are similar to enums in other languages" <|
            \() ->
                C
                    |> Expect.equal C
        , test "more complex types can be built with a 'tag' and additional data" <|
            \() ->
                Base C
                    |> Expect.equal (Base C)
        , test "all types in the union type are the same type" <|
            \() ->
                (Strand [ A, T, C, G ])
                    |> Expect.notEqual (Base A)
        , test "case statements may be used to extract the data from the type" <|
            case Base A of
                Strand nucleotides ->
                    \() ->
                        Expect.fail "Should not have hit this section"

                Base nucleotide ->
                    \() ->
                        A
                            |> Expect.equal nucleotide
        , test "case statements may be used to extract the data from the type" <|
            case Strand [A, G, T] of
                Strand nucleotides ->
                    \() ->
                        [A, G, T]
                            |> Expect.equal nucleotides

                Base nucleotide ->
                    \() ->
                        Expect.fail "Should not have hit this section"
        ]
