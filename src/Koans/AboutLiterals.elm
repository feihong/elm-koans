module AboutLiterals exposing (testSuite)

import Expect
import Test exposing (describe, test)
import TestHelpers exposing (..)

testSuite : Test.Test
testSuite =
    describe "About Literals"
        [ test "strings are enclosed in double quotes" <|
            \() ->
                "A string"
                    |> Expect.equal "A string"
        , test "characters are enclosed in single quotes" <|
            \() ->
                'A'
                    |> Expect.equal 'A'
        , test "floats have a decimal" <|
            \() ->
                42.24
                    |> Expect.equal 42.24
        , test "integers do not" <|
            \() ->
                42
                    |> Expect.equal 42
        , test "number literals can be integers" <|
            let
                num : Int
                num =
                    42
            in
                \() ->
                    42
                        |> Expect.equal num
        , test "number literals can be floats" <|
            let
                num : Float
                num =
                    42.0
            in
                \() ->
                    42.000
                        |> Expect.equal num
        , test "lists are denoted by brackets" <|
            \() ->
                -- Note that these are floats!
                [1.0, 2.0, 3.0]
                    |> Expect.equal [ 1, 2, 3 ]
        ]
