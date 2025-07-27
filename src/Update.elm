module Update exposing (..)

import Time
import Model exposing (Model , increment)
import DateEx exposing (toDateTriple)
import Process
import String
import Task

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
    ( { year = 2025, month = 1, day = 1, today = ( 2025, 1, 1 ), zone = Time.utc, autoplay = False, speed = 1000 }
    , Task.perform Today Time.now
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( todayY, todayM, todayD ) =
            model.today

        delayTick : Cmd Msg
        delayTick =
            Process.sleep model.speed
                |> Task.perform (always Tick)
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
            -- if model.year > 2099 || not model.autoplay then
            if model.year == 2099 && model.month == 12 && model.day == 31 || not model.autoplay then
                ( model, Cmd.none )

            else
                ( increment model, delayTick )

        YearSelected selectedYear ->
            ( { model | year = String.toInt selectedYear |> Maybe.withDefault todayY }, Cmd.none )

        MonthSelected selectedMonth ->
            ( { model | month = String.toInt selectedMonth |> Maybe.withDefault todayM }, Cmd.none )

        DaySelected selectedDay ->
            ( { model | day = String.toInt selectedDay |> Maybe.withDefault todayD }, Cmd.none )

        AutoPlayChecked isChecked ->
            ( { model | autoplay = isChecked }
            , if isChecked then
                delayTick

              else
                Cmd.none
            )

        SpeedSelected selectedSpeed ->
            ( { model | speed = String.toFloat selectedSpeed |> Maybe.withDefault 1000 }, Cmd.none )