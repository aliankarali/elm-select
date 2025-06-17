module Select.Update exposing (update)

import Select.Config exposing (Config)
import Select.Messages exposing (..)
import Select.Models exposing (State)
import Task


update : Config msg item -> Msg item -> State -> ( State, Cmd msg )
update config msg model =
    let
        queryChangeCmd value =
            case config.onQueryChange of
                Nothing ->
                    Cmd.none

                Just constructor ->
                    Task.succeed value
                        |> Task.perform constructor
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnEsc ->
            ( { model | query = Nothing }
            , case config.onEsc of
                Nothing ->
                    Cmd.none

                Just escMessage ->
                    Task.succeed Nothing
                        |> Task.perform (\x -> escMessage)
            )

        OnDownArrow ->
            let
                newHighlightedItem =
                    case model.highlightedItem of
                        Nothing ->
                            Just 0

                        Just n ->
                            Just (n + 1)
            in
            ( { model | highlightedItem = newHighlightedItem }
            , case config.onArrowDown of
                Nothing ->
                    Cmd.none

                Just arrowDownMessage ->
                    Task.succeed Nothing
                        |> Task.perform (\x -> arrowDownMessage)
            )

        OnUpArrow ->
            let
                newHighlightedItem =
                    case model.highlightedItem of
                        Nothing ->
                            Nothing

                        Just 0 ->
                            Nothing

                        Just n ->
                            Just (n - 1)
            in
            ( { model | highlightedItem = newHighlightedItem }
            , case config.onArrowUp of
                Nothing ->
                    Cmd.none

                Just arrowUpMessage ->
                    Task.succeed Nothing
                        |> Task.perform (\x -> arrowUpMessage)
            )

        OnFocus ->
            let
                cmd =
                    case config.onFocus of
                        Nothing ->
                            Cmd.none

                        Just focusMessage ->
                            Task.succeed Nothing
                                |> Task.perform (\x -> focusMessage)
            in
            case config.emptySearch of
                True ->
                    if config.preserveQueryOnFocus then
                        ( model, cmd )

                    else
                        ( { model | query = Just "" }
                        , Cmd.batch
                            [ cmd
                            , if config.emptySearch then
                                queryChangeCmd ""

                              else
                                Cmd.none
                            ]
                        )

                False ->
                    ( model, cmd )

        OnBlur ->
            ( { model | query = Nothing }
            , case config.onBlur of
                Nothing ->
                    Cmd.none

                Just blurMessage ->
                    Task.succeed Nothing
                        |> Task.perform (\x -> blurMessage)
            )

        OnClear ->
            let
                cmd =
                    Task.succeed Nothing
                        |> Task.perform config.onSelect
            in
            ( { model | query = Nothing }, cmd )

        OnRemoveItem item ->
            let
                cmd =
                    case config.onRemoveItem of
                        Just onRemoveItem ->
                            Task.succeed item
                                |> Task.perform onRemoveItem

                        Nothing ->
                            Cmd.none
            in
            ( model, cmd )

        OnQueryChange value ->
            let
                transformedValue =
                    value
                        |> config.transformInput

                transformedQuery =
                    transformedValue
                        |> config.transformQuery

                cmd =
                    if not (String.isEmpty transformedQuery) || config.emptySearch then
                        queryChangeCmd transformedQuery

                    else
                        Cmd.none
            in
            ( { model | highlightedItem = Nothing, query = Just transformedValue }, cmd )

        OnSelect item ->
            let
                cmd =
                    Task.succeed (Just item)
                        |> Task.perform config.onSelect
            in
            ( { model | query = Nothing }, cmd )
