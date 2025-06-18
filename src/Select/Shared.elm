module Select.Shared exposing
    ( classNames
    , difference
    , inputAttributes
    , onClickWithoutPropagation
    , referenceAttr
    , referenceDataName
    , splitWithSeparators
    , uniqueBy
    )

import Array
import Html exposing (Attribute)
import Html.Attributes exposing (attribute, autocomplete, class)
import Html.Events exposing (keyCode, on, onFocus, onInput, preventDefaultOn, stopPropagationOn)
import Json.Decode as Decode
import Regex
import Select.Config exposing (Config)
import Select.Messages as Msg exposing (Msg)
import Select.Models exposing (State)
import Set exposing (Set)


type alias ClassNames =
    { clear : String
    , input : String
    , inputWrapper : String
    , menu : String
    , menuAnchor : String
    , menuItem : String
    , menuItemSelectable : String
    , multiInputItem : String
    , multiInputItemContainer : String
    , multiInputItemRemove : String
    , multiInputItemText : String
    , root : String
    , underline : String
    , underlineWrapper : String
    }


classNames : ClassNames
classNames =
    { root = "elm-select"
    , inputWrapper = "elm-select-input-wrapper"
    , input = "elm-select-input"
    , underline = "elm-select-input-underline"
    , underlineWrapper = "elm-select-input-underline-wrapper"
    , clear = "elm-select-clear"

    -- Multi input
    , multiInputItemContainer = "elm-select-multi-input-item-container"
    , multiInputItem = "elm-select-multi-input-item"
    , multiInputItemText = "elm-select-multi-input-item-text"
    , multiInputItemRemove = "elm-select-multi-item-remove"

    -- Menu
    , menu = "elm-select-menu"
    , menuAnchor = "elm-select-menu-anchor"
    , menuItem = "elm-select-menu-item"
    , menuItemSelectable = "elm-select-menu-item-selectable"
    }


referenceDataName : String
referenceDataName =
    "data-select-id"


referenceAttr : State -> Attribute msg2
referenceAttr model =
    attribute referenceDataName model.id


difference : List item -> List item -> List item
difference listA listB =
    List.filter (\x -> not <| List.member x listB) listA


inputAttributes : Config msg item -> State -> List item -> Maybe (List item) -> List (Html.Attribute msg)
inputAttributes config model selectedItems maybeMatchedItems =
    let
        promptAttrs : List (Html.Attribute msg)
        promptAttrs =
            if List.isEmpty selectedItems then
                config.promptAttrs

            else
                []

        -- item that will be selected if enter if pressed
        preselectedItem : Maybe item
        preselectedItem =
            maybeMatchedItems
                |> Maybe.andThen
                    (\matchedItems ->
                        case model.highlightedItem of
                            Nothing ->
                                List.head matchedItems

                            Just n ->
                                Array.fromList matchedItems
                                    |> Array.get (remainderBy (List.length matchedItems) n)
                    )
    in
    [ autocomplete False
    , attribute "autocorrect" "off" -- for mobile Safari
    , onBlurAttribute model |> Html.Attributes.map config.toMsg
    , onKeyUpAttribute preselectedItem |> Html.Attributes.map config.toMsg
    , onKeyPressAttribute preselectedItem |> Html.Attributes.map config.toMsg
    , onInput Msg.OnQueryChange |> Html.Attributes.map config.toMsg
    , onFocus (Msg.OnFocus selectedItems) |> Html.Attributes.map config.toMsg
    , referenceAttr model
    , class classNames.input
    ]
        ++ config.inputAttrs
        ++ promptAttrs


onClickWithoutPropagation : Msg item -> Attribute (Msg item)
onClickWithoutPropagation msg =
    Decode.succeed ( msg, False )
        |> stopPropagationOn "click"


onKeyPressAttribute : Maybe item -> Attribute (Msg item)
onKeyPressAttribute maybeItem =
    let
        fn : Int -> Decode.Decoder (Msg item)
        fn code =
            case code of
                -- Tab
                9 ->
                    maybeItem
                        |> Maybe.map (Decode.succeed << Msg.OnSelect)
                        |> Maybe.withDefault (Decode.fail "nothing selected")

                -- Enter
                13 ->
                    maybeItem
                        |> Maybe.map (Decode.succeed << Msg.OnSelect)
                        |> Maybe.withDefault (Decode.fail "nothing selected")

                _ ->
                    Decode.fail "not TAB or ENTER"
    in
    preventDefaultOn "keypress"
        (Decode.andThen fn keyCode
            |> Decode.map (\msg -> ( msg, True ))
        )


onKeyUpAttribute : Maybe item -> Attribute (Msg item)
onKeyUpAttribute maybeItem =
    let
        selectItem : Decode.Decoder (Msg item)
        selectItem =
            case maybeItem of
                Nothing ->
                    Decode.fail "not Enter"

                Just item ->
                    Decode.succeed (Msg.OnSelect item)

        fn : Int -> Decode.Decoder (Msg item)
        fn code =
            case code of
                13 ->
                    selectItem

                38 ->
                    Decode.succeed Msg.OnUpArrow

                40 ->
                    Decode.succeed Msg.OnDownArrow

                27 ->
                    Decode.succeed Msg.OnEsc

                _ ->
                    Decode.fail "not ENTER"
    in
    preventDefaultOn "keyup"
        (Decode.andThen fn keyCode
            |> Decode.map (\msg -> ( msg, True ))
        )


onBlurAttribute : State -> Attribute (Msg item)
onBlurAttribute state =
    let
        dataDecoder : Decode.Decoder String
        dataDecoder =
            Decode.at [ "relatedTarget", "attributes", referenceDataName, "value" ] Decode.string

        attrToMsg : String -> Msg item
        attrToMsg attr =
            if attr == state.id then
                Msg.NoOp

            else
                Msg.OnBlur

        blur : Decode.Decoder (Msg item)
        blur =
            Decode.maybe dataDecoder
                |> Decode.map (Maybe.map attrToMsg)
                |> Decode.map (Maybe.withDefault Msg.OnBlur)
    in
    on "focusout" blur


splitWithSeparators : List String -> String -> List String
splitWithSeparators separators phrase =
    if List.isEmpty separators then
        [ phrase ]

    else
        let
            separatorRegex : Regex.Regex
            separatorRegex =
                separators
                    |> String.join "|"
                    |> Regex.fromString
                    |> Maybe.withDefault Regex.never
        in
        Regex.split separatorRegex phrase
            |> List.map String.trim


uniqueBy : (a -> comparable) -> List a -> List a
uniqueBy f list =
    uniqueHelper f Set.empty list []


uniqueHelper : (a -> comparable) -> Set comparable -> List a -> List a -> List a
uniqueHelper f existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            let
                computedFirst : comparable
                computedFirst =
                    f first
            in
            if Set.member computedFirst existing then
                uniqueHelper f existing rest accumulator

            else
                uniqueHelper f (Set.insert computedFirst existing) rest (first :: accumulator)
