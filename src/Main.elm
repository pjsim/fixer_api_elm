module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode


-- Need some styling


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
    ( Model 1.0 "AUD" "USD" "", Cmd.none )


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
    div [ id "input-stuff" ]
        [ div [ id "currency-converter" ]
            [ label [ for "input-amount" ] [ text "Amount" ]
            , input [ id "input-amount", placeholder "($)", onInput (InputChanged "amount"), model.amount |> toString |> defaultValue ] []
            , viewValidation model
            , label [ for "base-currency-select" ] [ text "Base Currency" ]
            , select [ id "base-currency-select", onInput (InputChanged "base_currency") ] (List.map (\x -> option [ x == model.base_currency |> selected ] [ text x ]) currencies)
            , label [ for "target-currency-select" ] [ text "Target Currency" ]
            , select [ id "target-currency-select", onInput (InputChanged "target_currency") ] (List.map (\x -> option [ x == model.target_currency |> selected ] [ text x ]) currencies)
            ]
        ]


validationStyle : String -> Attribute msg
validationStyle color =
    style
        [ ( "color", color )
        ]


viewValidation : Model -> Html Msg
viewValidation model =
    let
        ( color, message ) =
            if isNaN model.amount then
                ( "red", "Amount is an invalid number" )
            else if model.amount == 0.0 then
                ( "red", "Amount must be a positive number" )
            else if model.amount /= abs model.amount then
                ( "red", "Amount is a negative number" )
            else
                ( "", "" )
    in
        div [ validationStyle color ] [ text message ]


resultDisplay : Model -> Html Msg
resultDisplay model =
    div [ id "result-stuff" ]
        [ div [ id "actions" ]
            [ button [ onClick FetchFromApi ] [ text "Convert" ]
            ]
        , div [ id "result" ]
            [ text model.result
            ]
        ]


view : Model -> Html Msg
view model =
    div [ id "wrapper" ]
        [ inputForm model
        , resultDisplay model
        ]
