module Select.Select.Item exposing
    ( view
    , viewNotFound
    )

import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onMouseDown)
import Select.Config exposing (Config)
import Select.Messages exposing (Msg(..))
import Select.Models exposing (State)
import Select.Shared exposing (classNames, referenceAttr)


view : Config msg item -> State -> Int -> List item -> Int -> item -> Html msg
view config state itemCount selectedItems index item =
    let
        highlightedItemAttrs : List (Html.Attribute msg)
        highlightedItemAttrs =
            case state.highlightedItem of
                Nothing ->
                    []

                Just highlighted ->
                    -- take remainder as item numbers wrap around
                    if remainderBy itemCount highlighted == index then
                        config.highlightedItemAttrs

                    else
                        []

        selectedAttrs : List (Html.Attribute msg)
        selectedAttrs =
            if isSelected then
                config.selectedItemAttrs

            else
                []

        isSelected : Bool
        isSelected =
            List.member item selectedItems

        label : String
        label =
            config.toLabel item

        itemHtml : Html msg
        itemHtml =
            case config.itemHtml of
                Nothing ->
                    text label

                Just fn ->
                    fn state.query item
    in
    div
        ([ class classNames.menuItem
         , class classNames.menuItemSelectable
         , attribute "data-select-item" label
         , onMouseDown (config.toMsg (OnSelect item))
         , referenceAttr state
         ]
            ++ highlightedItemAttrs
            ++ selectedAttrs
        )
        [ itemHtml
        ]


viewNotFound : Config msg item -> Html msg
viewNotFound config =
    if config.notFound == "" then
        text ""

    else
        div
            (class classNames.menuItem :: config.notFoundAttrs)
            [ text config.notFound ]
