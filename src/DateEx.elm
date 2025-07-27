module DateEx exposing (..)

import Array exposing (fromList)
import Date exposing (monthToNumber)
import Time exposing (Month(..), Zone)


toDateTriple : Zone -> Time.Posix -> ( Int, Int, Int )
toDateTriple zone posix =
    ( Time.toYear zone posix
    , Time.toMonth zone posix |> monthToNumber
    , Time.toDay zone posix
    )


isLeapYear : Int -> Bool
isLeapYear year =
    modBy 4 year == 0 && modBy 100 year /= 0 || modBy 400 year == 0


lastDayOfMonth : Int -> Int -> Int
lastDayOfMonth year month =
    let
        daysInFeb =
            if isLeapYear year then
                29

            else
                28

        daysInMonth =
            fromList [ 31, daysInFeb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
    in
    daysInMonth
        |> Array.get (month - 1)
        |> Maybe.withDefault 31


increment : Int -> Int -> Int -> ( Int, Int, Int )
increment year month day =
    if day < lastDayOfMonth year month then
        ( year, month, day + 1 )

    else if month < 12 then
        ( year, month + 1, 1 )

    else
        ( year + 1, 1, 1 )
