module DateEx exposing (..)

import Date exposing (monthToNumber)
import Time exposing (Month(..), Zone)


toDateTriple : Zone -> Time.Posix -> ( Int, Int, Int )
toDateTriple zone posix =
    ( Time.toYear zone posix, Time.toMonth zone posix |> monthToNumber, Time.toDay zone posix )


lastDayOfmonth : Int -> Int -> Int
lastDayOfmonth y month =
    let
        isLeapYear =
            modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0
    in
    case month of
        2 ->
            if isLeapYear then
                29

            else
                28

        4 ->
            30

        6 ->
            30

        9 ->
            30

        11 ->
            30

        _ ->
            31
