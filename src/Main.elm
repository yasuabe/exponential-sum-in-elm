-- Show an analog clock for your time zone.
--
-- Dependencies:
--   elm install elm/svg
--   elm install elm/time
--
-- For a simpler version, check out:
--   https://elm-lang.org/examples/time
--


module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Date exposing (day, fromOrdinalDate, month, monthToNumber)
import Html exposing (Html, div)
import Html.Attributes as A
import Process
import String exposing (fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Tuple exposing (first, second)


tickIntervalMs : Float
tickIntervalMs =
    1000


year : Int
year =
    2025



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, delayTick )



-- UPDATE


type Msg
    = Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg count =
    case msg of
        Tick ->
            let
                newCommand =
                    if count >= 7 then
                        Cmd.none

                    else
                        delayTick
            in
            ( count + 1, newCommand )


delayTick : Cmd Msg
delayTick =
    Process.sleep tickIntervalMs
        |> Task.perform (always Tick)



-- VIEW


view : Model -> Html Msg
view count =
    let
        ( yy, mm, dd ) =
            ordinalToYymd count
    in
    div []
        [ div []
            [ text <| fromInt count ++ " - " ++ fromInt yy ++ "-" ++ fromInt mm ++ "-" ++ fromInt dd ]
        , div [ A.style "width" "400px", A.style "height" "400px" ]
            [ svg
                [ viewBox "0 0 400 400"

                , width "400"
                , height "400"
                ]
                [ makePolygon 25 3 28
                , polyline 
                    [ fill "none"
                    , points "0,0 0,400 400,400 400,0 0,0"
                    , stroke "#0000FF"
                    ]
                    []
                ]
            ]
        ]



-- SHAPE


makePolygon : Int -> Int -> Int -> Svg Msg
makePolygon yy mm dd =
    polyline
        [ fill "none"
        , points <| encodeExpSums yy mm dd
        , stroke "#FF0000"
        ]
        []


intPairsToPoints : List ( Float, Float ) -> String
intPairsToPoints pairs =
    String.join " " (List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y) pairs)



-- CALCULATE


mod : Int -> Int -> Int
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


scanExpSums : Int -> Int -> Int -> List ( Float, Float )
scanExpSums yy m d =
    let
        bound =
            2 * lcm3 yy m d + 1

        curr =
            calcExpSum ( yy, m, d )
    in
    List.range 0 bound
        |> scanl (\prev n -> add2 prev (curr n)) ( 0.0, 0.0 )
        |> adjust 400 400


encodeExpSums : Int -> Int -> Int -> String
encodeExpSums yy m d =
    scanExpSums yy m d |> intPairsToPoints


ordinalToYymd : Int -> ( Int, Int, Int )
ordinalToYymd ordinal =
    fromOrdinalDate year ordinal
        |> (\d -> ( mod year 100, month d |> monthToNumber, day d ))



-- ADJUST


adjust : Int -> Int -> List ( Float, Float ) -> List ( Float, Float )
adjust width height points =
    let
        ( viewCenterX, viewCenterY ) =
            ( toFloat width / 2.0, toFloat height / 2.0 )

        minX =
            List.minimum (List.map first points) |> Maybe.withDefault 0

        maxX =
            List.maximum (List.map first points) |> Maybe.withDefault 0

        minY =
            List.minimum (List.map second points) |> Maybe.withDefault 0

        maxY =
            List.maximum (List.map second points) |> Maybe.withDefault 0

        ( originalWidth, originalHeight ) =
            ( maxX - minX, maxY - minY )

        ( originalCenterX, originalCenterY ) =
            ( (minX + maxX) / 2, (minY + maxY)/ 2 )

        ratio =
            if originalWidth < originalHeight then
                -- 2.0
                toFloat height / originalHeight

            else
                -- 2.0

                toFloat width / originalWidth

        positionAdjusted =
            List.map
                (\( x, y ) ->
                    ( viewCenterX + (x - originalCenterX) * ratio, viewCenterY - (y - originalCenterY) * ratio )
                )
                points

        _ =
            Debug.log (
                "(" ++ f2s minX ++ ", " ++ f2s minY ++ "), (" ++ f2s maxX ++ ", " ++ f2s maxY ++ ") ------- " ++
                "(" ++ f2s originalWidth  ++ ", " ++ f2s originalHeight++ "), (" ++ f2s originalCenterX++ ", " ++ f2s originalCenterY ++ ") ------- " ++
                "(ratio=" ++ f2s ratio ++ ") ------- " ++
                "(" ++ f2s (viewCenterX + (minX - originalCenterX) * ratio) ++ ", " ++ f2s (viewCenterY + (minY - originalCenterY) * ratio)++ ") ------- " ++
                "(" ++ f2s (viewCenterX + (maxX - originalCenterX) * ratio) ++ ", " ++ f2s (viewCenterY + (maxY - originalCenterY) * ratio)++ ") ------- " ++
                "") ()
    in
    positionAdjusted


f2s : Float -> String
f2s f =
    f * 100 |> round |> toFloat |> (\n -> n / 100.0) |> String.fromFloat
