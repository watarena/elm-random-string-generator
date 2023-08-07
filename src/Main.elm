module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick, onCheck)
import Html exposing (label)
import Html.Attributes exposing (id)
import Html.Attributes exposing (for)
import Html.Attributes exposing (style)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Platform.Cmd as Cmd
import Random
import Html.Attributes exposing (checked)

-- MAIN
main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }

-- MODEL
type alias Model =
  {
    lowerAlphabets: Bool,
    upperAlphabets: Bool,
    numbers: Bool,
    symbols: Bool,
    custom: Bool,
    customContent: String,
    randomStringLength: Int,
    randomString: String
  }

modelToString : Model -> String
modelToString model = Debug.toString model

init : flags -> (Model, Cmd Msg )
init = \_ ->
  (
    {
      lowerAlphabets = True,
      upperAlphabets = True,
      numbers = False,
      symbols = False,
      custom = False,
      customContent = "",
      randomStringLength = 16,
      randomString = ""
    },
    Cmd.none
  )


-- UPDATE

type CheckboxMsg =
  CheckBoxLowerAlphabets(Bool)
  | CheckBoxUpperAlphabets(Bool)
  | CheckboxNumbers(Bool)
  | CheckBoxCustom(Bool)

updateCheckBox : CheckboxMsg -> Model -> Model
updateCheckBox msg model =
  case msg of
    CheckBoxLowerAlphabets b -> { model | lowerAlphabets = b }
    CheckBoxUpperAlphabets b -> { model | upperAlphabets = b }
    CheckboxNumbers b -> { model | numbers = b }
    CheckBoxCustom b -> { model | custom = b }

type Msg =
  CheckboxMsg(CheckboxMsg)
  | ChangeCustom(String)
  | ChangeLength(String)
  | GenerateRandomString
  | RandomString(String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CheckboxMsg cmsg -> (updateCheckBox cmsg model, Cmd.none)
    ChangeCustom s -> ({ model | customContent = s }, Cmd.none)
    ChangeLength s ->
      case String.toInt s of
          Nothing -> (model , Cmd.none)
          Just i -> ({model | randomStringLength = i}, Cmd.none)
    GenerateRandomString -> 
      let generator = randomStringGenerator (getAvailableChars model) model.randomStringLength in
      (model, Random.generate RandomString generator)
    RandomString s -> ({model | randomString = s}, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [
    style "margin" "10px"
  ] [
      checkBox CheckBoxLowerAlphabets "Lower Alphabets" model.lowerAlphabets,
      checkBox CheckBoxUpperAlphabets "Upper Alphabets" model.upperAlphabets,
      checkBox CheckboxNumbers "Numbers" model.numbers,
      checkBox CheckBoxCustom "custom" model.custom,
      input [ style "display" (if model.custom then "inline" else "none"), value model.customContent, onInput ChangeCustom ] [],
      div [] [
        text "Length:",
        input [ type_ "number", Html.Attributes.min "1", value <| String.fromInt model.randomStringLength, onInput ChangeLength ] []
      ],
      div [] [ text <| "Available characters: " ++ getAvailableChars model ],
      button [onClick GenerateRandomString] [ text "Generate!" ],
      div [] [
        div [
          style "visibility" (if String.length model.randomString > 0 then "visible" else "hidden"),
          style "border" "solid 1px",
          style "border-radius" "10px",
          style "display" "inline-block",
          style "padding" "3px 10px",
          style "margin" "5px 0px"
        ] [ text model.randomString ]
      ]
    ]

checkBox : (Bool -> CheckboxMsg) -> String -> Bool -> Html Msg
checkBox onCheckCallBack labelText isChecked =
  div []
    [
      input [ onCheck (onCheckCallBack >> CheckboxMsg), type_ "checkbox", id labelText, checked isChecked ] [],
      label [ for labelText ] [ text labelText ]
    ]

lowerAlphabets = "abcdefghijklmnopqrstuvwxyz"
upperAlphabets = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
numbers = "0123456789"

getAvailableChars : Model -> String
getAvailableChars model =
  let baseChars = String.concat
        [
          if model.lowerAlphabets then lowerAlphabets else "",
          if model.upperAlphabets then upperAlphabets else "",
          if model.numbers then numbers else ""
        ]
    in
      if model.custom then
        String.foldl (\c acc ->
            let cs = String.fromChar c in
              if String.contains cs acc then acc else acc ++ cs
          ) baseChars model.customContent
      else
        baseChars

getNthCharAsString str i = String.slice i (i+1) str
genetateStringFromIndeces str indexList =
  List.map (getNthCharAsString str) indexList |> String.concat

randomStringGenerator availableChars len =
  let randomGenerator = Random.int 0 (String.length availableChars - 1) in
  Random.list len randomGenerator
  |> Random.andThen (
    \lst -> genetateStringFromIndeces availableChars lst |> Random.constant
  )
