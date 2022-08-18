module Calc exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Model =
    { result : Int
    , input : Maybe Int
    , display : String
    , operator : Op
    }


type Op
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Eq
    | None


type Msg
    = InputDigit Int
    | InputOp Op
    | InputEqual
    | InputClear
    | InputAllClear


init : Model
init =
    { result = 0
    , input = Nothing
    , display = ""
    , operator = None
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputDigit num ->
            pushDigit num model

        InputOp op ->
            pushOperator op model

        InputEqual ->
            pushEqual model

        InputClear ->
            { model
                | input = Nothing
                , display = ""
            }

        InputAllClear ->
            init


pushDigit : Int -> Model -> Model
pushDigit number model =
    if model.result /= 0 && (model.operator == None || model.operator == Eq) then
        model

    else if model.input == Nothing then
        inputNumberSub number model

    else
        inputNumberSub (unwrapMaybeInt model.input * 10 + number) model


inputNumberSub : Int -> Model -> Model
inputNumberSub number model =
    { model
        | input = Just number
        , display = String.fromInt number
    }


{-| 演算子が押された際に考慮すべきケース

1.  modelがinitと同じ状態の場合
2.  数字入力後に初めて演算子が押された場合
3.  2度目以降の数字入力後に演算子が押された場合
4.  イコールの後に追加で演算子が押された場合と、演算子を押した後に別の演算子に変更する場合
5.  上記の条件を抜けてきてmodelのinputが-1の場合
6.  概ね計算実行ができる場合

-}
unwrapMaybeInt : Maybe Int -> Int
unwrapMaybeInt maybeinput =
    case maybeinput of
        Just item ->
            item

        Nothing ->
            0


pushOperator : Op -> Model -> Model
pushOperator op model =
    if model == init then
        model

    else if model.result == 0 && model.input /= Nothing then
        { model
            | result = unwrapMaybeInt model.input
            , input = Nothing
            , display = String.fromInt (unwrapMaybeInt model.input) ++ stringFromOp op
            , operator = op
        }

    else if model.result /= 0 && model.input /= Nothing then
        calcExecute op model

    else if model.operator == Eq || model.operator /= op then
        { model
            | display = model.display ++ stringFromOp op
            , operator = op
        }

    else if model.input == Nothing then
        model

    else
        calcExecute op model


pushEqual : Model -> Model
pushEqual model =
    if model.operator == None then
        model

    else if model.input == Nothing then
        case model.operator of
            Mul ->
                if model.display == "0" then
                    calcExecute Eq model

                else
                    model

            Div ->
                if model.display == "0" then
                    calcExecute Eq model

                else
                    model

            _ ->
                model

    else
        calcExecute Eq model


calcExecute : Op -> Model -> Model
calcExecute op model =
    let
        inputnum =
            unwrapMaybeInt model.input
    in
    case model.operator of
        Add ->
            calcSub (model.result + inputnum) op model

        Sub ->
            if model.result < inputnum then
                calcSub 0 op model

            else
                calcSub (model.result - inputnum) op model

        Mul ->
            calcSub (model.result * inputnum) op model

        Div ->
            calcSub (model.result // inputnum) op model

        Mod ->
            calcSub (remainderBy inputnum model.result) op model

        _ ->
            model


calcSub : Int -> Op -> Model -> Model
calcSub result op model =
    { model
        | result = result
        , input = Nothing
        , display = String.fromInt result ++ stringFromOp op
        , operator = op
    }


stringFromOp : Op -> String
stringFromOp op =
    case op of
        Add ->
            " + "

        Sub ->
            " - "

        Mul ->
            " * "

        Div ->
            " / "

        Mod ->
            " % "

        _ ->
            ""


view : Model -> Html Msg
view model =
    div []
        [ div [ class "display" ] [ text model.display ]
        , div []
            [ button [ onClick InputAllClear ] [ text "AC" ]
            , button [ onClick InputClear ] [ text "C" ]
            , button [ onClick (InputOp Mod) ] [ text "%" ]
            ]
        , div []
            [ button [ onClick (InputDigit 7) ] [ text "7" ]
            , button [ onClick (InputDigit 8) ] [ text "8" ]
            , button [ onClick (InputDigit 9) ] [ text "9" ]
            , button [ onClick (InputOp Add) ] [ text "+" ]
            , button [ onClick (InputOp Sub) ] [ text "-" ]
            ]
        , div []
            [ button [ onClick (InputDigit 4) ] [ text "4" ]
            , button [ onClick (InputDigit 5) ] [ text "5" ]
            , button [ onClick (InputDigit 6) ] [ text "6" ]
            , button [ onClick (InputOp Mul) ] [ text "*" ]
            , button [ onClick (InputOp Div) ] [ text "/" ]
            ]
        , div []
            [ button [ onClick (InputDigit 0) ] [ text "0" ]
            , button [ onClick (InputDigit 1) ] [ text "1" ]
            , button [ onClick (InputDigit 2) ] [ text "2" ]
            , button [ onClick (InputDigit 3) ] [ text "3" ]
            , button [ onClick InputEqual ] [ text "=" ]
            ]
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
