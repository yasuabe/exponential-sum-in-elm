module View exposing (..)

import DateEx exposing (lastDayOfmonth)
import Html exposing (Html, div, input, label, select, span, text)
import Html.Attributes as A exposing (checked, class, for, id, selected, style)
import Html.Events as A
import Model exposing (Model, scanExpSums)
import String
import Svg exposing (Svg, polyline, svg)
import Svg.Attributes exposing (fill, height, points, stroke, viewBox, width)
import Tuple exposing (first, second)
import Update exposing (Msg(..))



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "display" "flex", style "flex-direction" "row", style "align-items" "center" ]
        [ div [ A.style "width" "400px", A.style "height" "400px" ]
            [ svg
                [ viewBox "0 0 400 400"
                , width "400"
                , height "400"
                ]
                [ makePolygon (modBy 100 model.year) model.month model.day
                , polyline
                    [ fill "none"
                    , points "0,0 0,400 400,400 400,0 0,0"
                    , stroke "#0000FF"
                    ]
                    []
                ]
            ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "column"
            ]
            [ viewYearSelector model.year
            , viewMonthSelector model
            , viewDaySelector model
            , viewAutoplayToggle model
            , viewSpeedSelect model
            ]
        ]


viewYearSelector : Int -> Html Msg
viewYearSelector currentYear =
    div
        []
        [ label [ for "year-selector", style "font-size" "14px" ] [ text "Year" ]
        , select [ id "year-selector", A.style "width" "100px", A.style "height" "30px", A.onInput YearSelected ]
            (List.map
                (\y ->
                    Html.option
                        [ A.value (String.fromInt y), selected <| currentYear == y ]
                        [ text (String.fromInt y) ]
                )
                (List.range 2001 2099)
            )
        ]


viewMonthSelector : Model -> Html Msg
viewMonthSelector model =
    div
        []
        [ label [ for "year-selector", style "font-size" "14px" ] [ text "Month" ]
        , select [ A.style "width" "100px", A.style "height" "30px", A.onInput MonthSelected ]
            (List.map
                (\m ->
                    Html.option
                        [ A.value (String.fromInt m), selected <| model.month == m ]
                        [ text (String.fromInt m) ]
                )
                (List.range 1 12)
            )
        ]


viewDaySelector : Model -> Html Msg
viewDaySelector model =
    div
        []
        [ label [ for "year-selector", style "font-size" "14px" ] [ text "Day" ]
        , select [ A.style "width" "100px", A.style "height" "30px", A.onInput DaySelected ]
            (List.map
                (\d ->
                    Html.option
                        [ A.value (String.fromInt d), selected <| model.day == d ]
                        [ text (String.fromInt d) ]
                )
                (List.range 1 <| lastDayOfmonth model.year model.month)
            )
        ]


viewAutoplayToggle : Model -> Html Msg
viewAutoplayToggle model =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "margin" "3px auto auto"
        ]
        [ span [ style "margin" "auto 7px 2px", style "font-size" "12px" ] [ text "AUTO" ]
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
        [ span [ style "margin" "auto 7px 2px", style "font-size" "12px" ] [ text "SPEED" ]
        , select [ A.style "width" "100px", A.style "height" "30px", A.onInput SpeedSelected ]
            (List.map
                (\( v, t ) -> Html.option [ A.value v, selected <| v == String.fromFloat model.speed ] [ text t ])
                [ ( "250", "0.25s" )
                , ( "500", "0.5s" )
                , ( "1000", "1s" )
                , ( "2000", "2s" )
                , ( "4000", "4s" )
                ]
            )
        ]



-- SHAPE


makePolygon : Int -> Int -> Int -> Svg msg
makePolygon yy mm dd =
    let
        date =
            "(date: " ++ String.fromInt yy ++ "-" ++ String.fromInt mm ++ "-" ++ String.fromInt dd ++ ")"

        pts =
            encodeExpSums yy mm dd

        _ =
            Debug.log (date ++ "    " ++ pts) ()
    in
    polyline
        [ fill "none"
        , points pts
        , stroke "#FF0000"
        ]
        []


encodeExpSums : Int -> Int -> Int -> String
encodeExpSums yy m d =
    scanExpSums yy m d |> adjust 400 400 |> floatPairsToPoints


floatPairsToPoints : List ( Float, Float ) -> String
floatPairsToPoints pairs =
    String.join " " (List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y) pairs)


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
            ( (minX + maxX) / 2, (minY + maxY) / 2 )

        ratio =
            if originalWidth < originalHeight then
                toFloat height / originalHeight

            else
                toFloat width / originalWidth

        positionAdjusted =
            List.map
                (\( x, y ) ->
                    ( viewCenterX + (x - originalCenterX) * ratio, viewCenterY - (y - originalCenterY) * ratio )
                )
                points
    in
    positionAdjusted
