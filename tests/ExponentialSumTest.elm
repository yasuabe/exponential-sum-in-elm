module ExponentialSumTest exposing (..)

import Expect
import View
import Test exposing (..)


suit : Test
suit =
    describe "ExponentialSum"
        [ test "degration stopper for refactoring" <|
            \() ->
                View.encodeExpSums 25 3 26
                    |> String.left 100
                    |> (Expect.equal <| String.left 100 expected2)
        ]


expected2 : String
expected2 =
    "264.06647466976415,78.58323241581222 277.5490823588306,86.92779669774518 261.69309096275003,86.92779"
