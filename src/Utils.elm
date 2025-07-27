module Utils exposing (..)

mod :
    Int
    -> Int
    -> Int -- TODO: check if this is needed
mod x y =
    Basics.modBy y x


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b (mod a b)


lcm : Int -> Int -> Int
lcm a b =
    abs (a * b) // gcd a b


lcm3 : Int -> Int -> Int -> Int
lcm3 a b c =
    lcm (lcm a b) c


add2 : ( number, number ) -> ( number, number ) -> ( number, number )
add2 ( a1, a2 ) ( b1, b2 ) =
    ( a1 + b1, a2 + b2 )


mapThree : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapThree f ( x, y, z ) =
    ( f x, f y, f z )


calcExpSum : ( Int, Int, Int ) -> Int -> ( Float, Float )
calcExpSum yymd index =
    ( mapThree toFloat yymd, toFloat index )
        |> (\( ( y, m, d ), n ) -> n / m + (n ^ 2) / d + (n ^ 3) / y)
        |> (*) (2 * pi)
        |> (\t -> ( cos t, sin t ))


scanl : (b -> a -> b) -> b -> List a -> List b
scanl f q ls =
    let
        g a bs =
            case bs of
                [] ->
                    [ f q a ]

                x :: xs ->
                    f x a :: x :: xs
    in
    List.foldl g [] ls