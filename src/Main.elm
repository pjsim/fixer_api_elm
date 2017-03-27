module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { amount : Float
    , base_currency : String
    , target_currency : String
    , result : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0.0 "AUD" "USD" "", Cmd.none )


type Msg
    = FetchFromApi
    | DisplayConversion (Result Http.Error String)
    | InputChanged String String


apiUrl : String
apiUrl =
    "https://api.fixer.io/latest"


fullApiUrl : Model -> String
fullApiUrl model =
    apiUrl ++ "?base=" ++ model.base_currency ++ "&symbols=" ++ model.target_currency



-- UPDATE


getJsonData : Model -> Http.Request String
getJsonData model =
    model
        |> fullApiUrl
        |> Http.getString


getJsonDataCmd : Model -> Cmd Msg
getJsonDataCmd model =
    model
        |> getJsonData
        |> Http.send DisplayConversion


getJsonDataCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
getJsonDataCompleted model result =
    case result of
        Ok newConversion ->
            ( { model | result = convertFromApi newConversion model }, Cmd.none )

        Err _ ->
            ( model, Cmd.none )


convertFromApi : String -> Model -> String
convertFromApi result model =
    let
        newResult =
            Decode.decodeString (Decode.field "rates" (Decode.field model.target_currency Decode.float)) result
    in
        if model.amount == 0 || abs model.amount /= model.amount then
            "There was an error doing this conversion"
        else
            case newResult of
                Ok result ->
                    (toString model.amount) ++ " " ++ model.base_currency ++ " is equal to " ++ toString (model.amount * result) ++ " " ++ model.target_currency

                Err _ ->
                    "There was an error doing this conversion"


updateInput : Model -> String -> String -> ( Model, Cmd Msg )
updateInput model valueType changedValue =
    case valueType of
        "amount" ->
            case String.toFloat changedValue of
                Ok value ->
                    ( { model | amount = value }, Cmd.none )

                Err _ ->
                    ( { model | amount = 0 }, Cmd.none )

        "base_currency" ->
            ( { model | base_currency = changedValue }, Cmd.none )

        "target_currency" ->
            ( { model | target_currency = changedValue }, Cmd.none )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchFromApi ->
            ( model, getJsonDataCmd model )

        DisplayConversion result ->
            getJsonDataCompleted model result

        InputChanged valueType changedValue ->
            updateInput model valueType changedValue



-- VIEW


currencies : List String
currencies =
    [ "EUR", "AUD", "BGN", "BRL", "CAD", "CHF", "CNY", "CZK", "DKK", "GBP", "HKD", "HRK", "HUF", "IDR", "ILS", "INR", "JPY", "KRW", "MXN", "MYR", "NOK", "NZD", "PHP", "PLN", "RON", "RUB", "SEK", "SGD", "THB", "TRY", "USD", "ZAR" ]


inputForm : Model -> Html Msg
inputForm model =
    Html.form [ class "well" ]
        [ div [ class "form-group" ]
            [ label [ for "input-amount" ] [ text "Amount" ]
            , input [ id "input-amount", class "form-control", onInput (InputChanged "amount"), placeholder "30" ] []
            , amountValidation model
            ]
        , div [ class "form-group" ]
            [ label [ for "base-currency-select" ] [ text "Base Currency" ]
            , select [ id "base-currency-select", class "form-control", onInput (InputChanged "base_currency") ] (List.map (\x -> option [ x == model.base_currency |> selected ] [ text x ]) currencies)
            ]
        , div [ class "form-group" ]
            [ label [ for "target-currency-select" ] [ text "Target Currency" ]
            , select [ id "target-currency-select", class "form-control", onInput (InputChanged "target_currency") ] (List.map (\x -> option [ x == model.target_currency |> selected ] [ text x ]) currencies)
            ]
        , hr [] []
        , button [ type_ "button", class "btn btn-default btn-block", onClick FetchFromApi ] [ text "Convert" ]
        ]


validationStyle : String -> Attribute msg
validationStyle color =
    style
        [ ( "color", color )
        ]


amountValidation : Model -> Html Msg
amountValidation model =
    let
        ( color, message ) =
            if isNaN model.amount then
                ( "red", "Amount is an invalid number" )
            else if (model.amount == 0.0 || model.amount /= abs model.amount) then
                ( "red", "Amount must be a positive number" )
            else
                ( "", "" )
    in
        div [ validationStyle color ] [ text message ]


resultDisplay : Model -> Html Msg
resultDisplay model =
    div []
        [ div [ class "panel panel-default" ]
            [ div [ class "panel-heading" ]
                [ h3 [ class "panel-title" ]
                    [ text "Fixer.io Currency Converter"
                    ]
                ]
            , div [ class "panel-body" ]
                [ text model.result
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "container", style [ ( "margin-top", "50px" ) ] ]
        [ div [ class "row" ]
            [ div [ class "col-xs-12 col-md-4 col-lg-3" ]
                [ inputForm model
                ]
            , div [ class "col-xs-12 col-md-8 col-lg-9" ]
                [ resultDisplay model
                ]
            ]
        ]
