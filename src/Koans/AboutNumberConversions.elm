module AboutNumberConversions exposing (testSuite)

import Expect
import Test exposing (describe, test)
import TestHelpers exposing (..)


testSuite =
    describe "About Number Conversions"
        [ test "toFloat converts an int to a float" <|
            \() ->
                5.0
                    |> Expect.equal (toFloat 5)
        , test "floor converts a float to an int (rounding down)" <|
            \() ->
                3
                    |> Expect.equal (floor 3.6)
        , test "floor converts a float to an int (rounding down)" <|
            \() ->
                -4
                    |> Expect.equal (floor -3.6)
        , test "ceiling converts a float to an int (rounding up)" <|
            \() ->
                4
                    |> Expect.equal (ceiling 3.6)
        , test "ceiling converts a float to an int (rounding up)" <|
            \() ->
                -3
                    |> Expect.equal (ceiling -3.6)
        , test "round converts a float to an int (rounding to the closest int)" <|
            \() ->
                4
                    |> Expect.equal (round 3.6)
        , test "round converts a float to an int (rounding to the closest int)" <|
            \() ->
                -4
                    |> Expect.equal (round -3.6)
        , test "truncate converts a float to an int (rounding towards 0)" <|
            \() ->
                3
                    |> Expect.equal (truncate 3.6)
        , test "truncate converts a float to an int (rounding towards 0)" <|
            \() ->
                -3
                    |> Expect.equal (truncate -3.6)
        ]
