module Update exposing (..)

import DateEx exposing (toDateTriple)
import Model exposing (..)
import Process
import Task
import Time


type Msg
    = Tick
    | Today Time.Posix
    | YearSelected String
    | MonthSelected String
    | DaySelected String
    | AutoPlayChecked Bool
    | SpeedSelected String


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Task.perform Today Time.now
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        withNone m =
            ( m, Cmd.none )

        delayTick =
            Process.sleep model.speed |> Task.perform (always Tick)
    in
    case msg of
        Today posix ->
            let
                ( year, month, day ) =
                    toDateTriple model.zone posix

                newModel =
                    { model | year = year, month = month, today = ( year, month, day ) }
            in
            ( newModel, delayTick )

        Tick ->
            if isAtMaxDate model || not model.autoplay then
                model |> withNone

            else
                ( incrementDate model, delayTick )

        YearSelected yyyy ->
            model |> setYearS yyyy |> withNone

        MonthSelected mm ->
            model |> setMonthS mm |> withNone

        DaySelected dd ->
            model |> setDayS dd |> withNone

        AutoPlayChecked checked ->
            ( { model | autoplay = checked }
            , if checked then
                delayTick

              else
                Cmd.none
            )

        SpeedSelected speed ->
            model |> setSpeedS speed |> withNone
