-- Show an analog clock for your time zone.
--
-- Dependencies:
--   elm install elm/svg
--   elm install elm/time
--
-- For a simpler version, check out:
--   https://elm-lang.org/examples/time
--


module Model exposing (Model, increment, scanExpSums)

import DateEx exposing (lastDayOfmonth)
import Time exposing (Month(..), Zone)
import Utils exposing (add2, calcExpSum, lcm3, scanl)



-- MODEL


type alias Model =
    { year : Int
    , month : Int
    , day : Int
    , today : ( Int, Int, Int )
    , zone : Zone
    , autoplay : Bool
    , speed : Float
    }



-- UPDATE


increment : Model -> Model
increment model =
    let
        daysInMonth =
            lastDayOfmonth model.year model.month

        ( monthInc, newDay ) =
            if model.day < daysInMonth then
                ( False, model.day + 1 )

            else
                ( True, 1 )

        ( yearInc, newMonth ) =
            if monthInc && model.month < 12 then
                ( False, model.month + 1 )

            else if monthInc then
                ( True, 1 )

            else
                ( False, model.month )

        newYear =
            if yearInc then
                model.year + 1

            else
                model.year
    in
    { model | year = newYear, month = newMonth, day = newDay }




-- CALCULATE


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
