module AboutTuples exposing (testSuite)

import Expect
import Test exposing (describe, test)
import TestHelpers exposing (..)
import Tuple


testSuite =
    describe "About Tuples"
        [ test "tuples are like lists of fixed length" <|
            \() ->
                (1, 2)
                    |> Expect.equal ( 1, 2 )
        , test "tuples may also be of mixed types" <|
            \() ->
                (1, "hey")
                    |> Expect.equal ( 1, "hey" )
        , test "there is a special comma syntax for creating tuples" <|
            \() ->
                (1, "hey")
                    |> Expect.equal ((,) 1 "hey")
        , test "you use as many commas as there would be in the tuple" <|
            \() ->
                (1, "hey", [])
                    |> Expect.equal ((,,) 1 "hey" [])
        , test "first gets the first element of a 2-tuple" <|
            \() ->
                'a'
                    |> Expect.equal (Tuple.first ('a', 'b'))
        , test "second gets the second element of a 2-tuple" <|
            \() ->
                'B'
                    |> Expect.equal (Tuple.second ('A', 'B'))
        , test "case statements may be used to destructure a tuple" <|
            \() ->
                case ( 1, 2 ) of
                    ( first, second ) ->
                        ((first == 1) && (second == 2))
                            |> Expect.true "Should be True"
        , test "tuples may also be destructured by function arguments" <|
            \() ->
                let func (a, b) =
                    a == 1 && b == 2
                in
                    ( 1, 2 )
                        |> func
                        |> Expect.true "Should be True"
        ]
