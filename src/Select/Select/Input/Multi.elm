module Select.Select.Input.Multi exposing (view)

import Html exposing (Html, div, input, text)
import Html.Attributes
    exposing
        ( class
        , placeholder
        , value
        )
import Select.Config exposing (Config)
import Select.Messages as Msg
import Select.Models exposing (State)
import Select.Select.RemoveItem as RemoveItem
import Select.Shared as Shared exposing (classNames)


view :
    Config msg item
    -> State
    -> List item
    -> List item
    -> Maybe (List item)
    -> List (Html msg)
view config model availableItems selected maybeMatchedItems =
    let
        val =
            model.query |> Maybe.withDefault ""
    in
    [ currentSelection
        config
        selected
    , input
        (Shared.inputAttributes config model availableItems selected maybeMatchedItems
            ++ (if List.isEmpty selected then
                    [ placeholder config.prompt ]

                else
                    []
               )
            ++ [ value val ]
        )
        []
    ]


currentSelection : Config msg item -> List item -> Html msg
currentSelection config selected =
    div
        (class classNames.multiInputItemContainer
            :: config.multiInputItemContainerAttrs
        )
        (List.map
            (currentSelectionItem config)
            selected
        )


currentSelectionItem : Config msg item -> item -> Html msg
currentSelectionItem config item =
    div
        (class classNames.multiInputItem
            :: config.multiInputItemAttrs
        )
        [ div
            [ class classNames.multiInputItemText ]
            [ text (config.toLabel item) ]
        , currentSelectionItemMaybeClear
            config
            item
        ]


currentSelectionItemMaybeClear : Config msg item -> item -> Html msg
currentSelectionItemMaybeClear config item =
    case config.onRemoveItem of
        Nothing ->
            text ""

        Just _ ->
            currentSelectionItemClear
                config
                item


currentSelectionItemClear : Config msg item -> item -> Html msg
currentSelectionItemClear config item =
    let
        removableHtml : Html msg
        removableHtml =
            div
                [ class classNames.multiInputItemRemove
                , Shared.onClickWithoutPropagation (Msg.OnRemoveItem item)
                    |> Html.Attributes.map config.toMsg
                ]
                [ RemoveItem.view config ]
    in
    case config.multiInputItemRemovable of
        Nothing ->
            removableHtml

        Just fn ->
            if fn item then
                removableHtml

            else
                div [] []
