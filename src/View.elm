module View exposing (..)

import DateEx exposing (lastDayOfMonth)
import Html exposing (Html, div, input, label, select, span, text)
import Html.Attributes as A exposing (checked, class, for, id, selected, style)
import Html.Events as A
import Model exposing (Model, scanExpSums)
import String exposing (fromFloat)
import Svg exposing (Svg, polyline, svg)
import Svg.Attributes exposing (fill, height, points, stroke, viewBox, width)
import Update exposing (Msg(..))
import Utils exposing (Pair, joinPair, map2Pair, mapPair, maxInt, mean, minInt, toPair)


view : Model -> Html Msg
view model =
    div [ style "display" "flex", style "flex-direction" "row", style "align-items" "center" ]
        [ div [ A.style "width" "400px", A.style "height" "400px" ]
            [ svg
                [ viewBox "0 0 400 400"
                , width "400"
                , height "400"
                ]
                [ createPolyline (modBy 100 model.year) model.month model.day
                , polyline
                    [ fill "none"
                    , points "0,0 0,400 400,400 400,0 0,0"
                    , stroke "#aaaabb"
                    ]
                    []
                ]
            ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "column"
            ]
            (List.map (\f -> f model)
                [ viewYearSelector
                , viewMonthSelector
                , viewDaySelector
                , viewAutoplayToggle
                , viewSpeedSelect
                ]
            )
        ]


viewYearSelector : Model -> Html Msg
viewYearSelector { year } =
    div
        []
        [ label [ for "year-selector", style "font-size" "14px" ] [ text "Year" ]
        , select [ id "year-selector", A.style "width" "100px", A.style "height" "30px", A.onInput YearSelected ]
            (List.map
                (\y ->
                    Html.option
                        [ A.value (String.fromInt y), selected <| year == y ]
                        [ text (String.fromInt y) ]
                )
                (List.range 2001 2099)
            )
        ]


viewMonthSelector : Model -> Html Msg
viewMonthSelector { month } =
    div
        []
        [ label [ for "month-selector", style "font-size" "14px" ] [ text "Month" ]
        , select [ id "month-selector", A.style "width" "100px", A.style "height" "30px", A.onInput MonthSelected ]
            (List.map
                (\m ->
                    Html.option
                        [ A.value (String.fromInt m), selected <| month == m ]
                        [ text (String.fromInt m) ]
                )
                (List.range 1 12)
            )
        ]


viewDaySelector : Model -> Html Msg
viewDaySelector { year, month, day } =
    div
        []
        [ label [ for "day-selector", style "font-size" "14px" ] [ text "Day" ]
        , select [ id "day-selector", A.style "width" "100px", A.style "height" "30px", A.onInput DaySelected ]
            (List.map
                (\d ->
                    Html.option
                        [ A.value (String.fromInt d), selected <| day == d ]
                        [ text (String.fromInt d) ]
                )
                (List.range 1 <| lastDayOfMonth year month)
            )
        ]


viewAutoplayToggle : Model -> Html Msg
viewAutoplayToggle model =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "margin" "3px auto auto"
        ]
        [ span [ style "margin" "auto 7px 2px", style "font-size" "12px" ] [ text "Autoplay" ]
        , div [ class "autoplay-toggle" ]
            [ input
                [ id "autoplay-toggle"
                , A.type_ "checkbox"
                , class "autoplay-toggle__checkbox"
                , checked model.autoplay
                , A.onCheck AutoPlayChecked
                ]
                []
            , label [ for "autoplay-toggle", class "autoplay-toggle__label" ] []
            ]
        ]


viewSpeedSelect : Model -> Html Msg
viewSpeedSelect model =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "margin" "3px auto auto"
        ]
        [ span [ style "margin" "auto 7px 2px", style "font-size" "12px" ] [ text "Speed" ]
        , select [ A.style "width" "100px", A.style "height" "30px", A.onInput SpeedSelected ]
            (List.map
                (\( v, t ) -> Html.option [ A.value v, selected <| v == fromFloat model.speed ] [ text t ])
                [ ( "250", "0.25s" )
                , ( "500", "0.5s" )
                , ( "1000", "1s" )
                , ( "2000", "2s" )
                , ( "4000", "4s" )
                ]
            )
        ]



-- SHAPE


type alias Point =
    Pair Float


createPolyline : Int -> Int -> Int -> Svg msg
createPolyline yy mm dd =
    polyline
        [ fill "none"
        , points <| generateExpSumsPoints yy mm dd
        , stroke "#FF0000"
        ]
        []


generateExpSumsPoints : Int -> Int -> Int -> String
generateExpSumsPoints yy m d =
    scanExpSums yy m d
        |> adjustPointsToView 400 400
        |> List.map (mapPair fromFloat >> joinPair ",")
        |> String.join " "


adjustPointsToView : Float -> Float -> List Point -> List Point
adjustPointsToView width height points =
    let
        ( viewCenterX, viewCenterY ) =
            mapPair (\x -> x / 2) ( width, height )

        ( ( minX, minY ), ( maxX, maxY ) ) =
            List.foldl
                (\p ( accMin, accMax ) -> ( map2Pair min p accMin, map2Pair max p accMax ))
                (mapPair (toFloat >> toPair) ( maxInt, minInt ))
                points

        ( originalW, originalH ) =
            ( maxX - minX, maxY - minY )

        ( centerX, centerY ) =
            ( mean minX maxX, mean minY maxY )

        ratio =
            if originalW < originalH then
                height / originalH

            else
                width / originalW
    in
    List.map
        (\( x, y ) -> ( viewCenterX + (x - centerX) * ratio, viewCenterY - (y - centerY) * ratio ))
        points
