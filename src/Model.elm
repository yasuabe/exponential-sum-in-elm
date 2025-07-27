module Model exposing
    ( Model
    , incrementDate
    , initialModel
    , isAtMaxDate
    , patchField
    , scanExpSums
    , setDayS
    , setMonthS
    , setSpeedS
    , setYearS
    )

import DateEx exposing (increment)
import Time exposing (Month(..), Zone)
import Utils exposing (add2, lcm3, map3, scanl)


type alias Model =
    { year : Int
    , month : Int
    , day : Int
    , today : ( Int, Int, Int )
    , zone : Zone
    , autoplay : Bool
    , speed : Float
    }


initialModel : Model
initialModel =
    { year = 2025
    , month = 1
    , day = 1
    , today = ( 2025, 1, 1 )
    , zone = Time.utc
    , autoplay = False
    , speed = 1000
    }


patchField : (a -> Model -> Model) -> (String -> Maybe a) -> a -> String -> Model -> Model
patchField setter converter default value model =
    setter (converter value |> Maybe.withDefault default) model


patchDateField : (Int -> Model -> Model) -> (Model -> Int) -> String -> Model -> Model
patchDateField setter defaultGetter value model =
    patchField setter String.toInt (defaultGetter model) value model


setYearS : String -> Model -> Model
setYearS =
    patchDateField (\v m -> { m | year = v }) .year


setMonthS : String -> Model -> Model
setMonthS =
    patchDateField (\v m -> { m | month = v }) .month


setDayS : String -> Model -> Model
setDayS =
    patchDateField (\v m -> { m | day = v }) .day


setSpeedS : String -> Model -> Model
setSpeedS speed =
    patchField (\v m -> { m | speed = v }) String.toFloat 1000 speed


isAtMaxDate : Model -> Bool
isAtMaxDate model =
    model.year == 2099 && model.month == 12 && model.day == 31


incrementDate : Model -> Model
incrementDate model =
    let
        ( yyyy, mm, dd ) =
            increment model.year model.month model.day
    in
    { model | year = yyyy, month = mm, day = dd }



-- EXPONENTIAL SUMS
-- https://www.johndcook.com/expsum/details.html


calcExpSum : ( Int, Int, Int ) -> Int -> ( Float, Float )
calcExpSum yymd index =
    ( map3 toFloat yymd, toFloat index )
        |> (\( ( y, m, d ), n ) -> n / m + (n ^ 2) / d + (n ^ 3) / y)
        |> (*) (2 * pi)
        |> (\t -> ( cos t, sin t ))


scanExpSums : Int -> Int -> Int -> List ( Float, Float )
scanExpSums yy mm dd =
    let
        bound =
            2 * lcm3 yy mm dd + 1

        curr =
            calcExpSum ( yy, mm, dd )
    in
    List.range 0 bound
        |> scanl (\n prev -> add2 prev (curr n)) ( 0.0, 0.0 )
