module Utils exposing (..)

-- MATH


minInt : Int
minInt =
    -2147483648


maxInt : Int
maxInt =
    2147483647


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b (modBy b a)


lcm : Int -> Int -> Int
lcm a b =
    abs (a * b) // gcd a b


lcm3 : Int -> Int -> Int -> Int
lcm3 a b =
    lcm (lcm a b)

mean : Float -> Float -> Float
mean a b =
    (a + b) / 2



-- LISTS


scanl : (a -> b -> b) -> b -> List a -> List b
scanl f q =
    let
        g a bs =
            case bs of
                [] ->
                    [ f a q ]

                x :: xs ->
                    f a x :: x :: xs
    in
    List.foldl g []



-- PAIRS


type alias Pair a =
    ( a, a )


mapPair : (a -> b) -> Pair a -> Pair b
mapPair f ( x, y ) =
    ( f x, f y )


map2Pair : (a -> a -> b) -> Pair a -> Pair a -> Pair b
map2Pair f ( x1, y1 ) ( x2, y2 ) =
    ( f x1 x2, f y1 y2 )


joinPair : String -> Pair String -> String
joinPair sep ( a, b ) =
    a ++ sep ++ b


toPair : a -> Pair a
toPair a =
    ( a, a )


add2 : Pair number -> Pair number -> Pair number
add2 ( a1, a2 ) ( b1, b2 ) =
    ( a1 + b1, a2 + b2 )


map3 : (a -> b) -> ( a, a, a ) -> ( b, b, b )
map3 f ( x, y, z ) =
    ( f x, f y, f z )
