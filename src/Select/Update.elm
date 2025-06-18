module Select.Update exposing (update)

import Select.Config exposing (Config)
import Select.Messages exposing (Msg(..))
import Select.Models exposing (State)
import Task


update : Config msg item -> Msg item -> State -> ( State, Cmd msg )
update config msg model =
    let
        queryChangeCmd : String -> Cmd msg
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
                        |> Task.perform (\_ -> escMessage)
            )

        OnDownArrow ->
            let
                newHighlightedItem : Maybe Int
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
                        |> Task.perform (\_ -> arrowDownMessage)
            )

        OnUpArrow ->
            let
                newHighlightedItem : Maybe Int
                newHighlightedItem =
                    model.highlightedItem
                        |> Maybe.andThen
                            (\n ->
                                if n == 0 then
                                    Nothing

                                else
                                    Just (n - 1)
                            )
            in
            ( { model | highlightedItem = newHighlightedItem }
            , case config.onArrowUp of
                Nothing ->
                    Cmd.none

                Just arrowUpMessage ->
                    Task.succeed Nothing
                        |> Task.perform (\_ -> arrowUpMessage)
            )

        OnFocus selectedItems ->
            let
                cmd : Cmd msg
                cmd =
                    case config.onFocus of
                        Nothing ->
                            Cmd.none

                        Just focusMessage ->
                            Task.succeed Nothing
                                |> Task.perform (\_ -> focusMessage)
            in
            if config.emptySearch && (not config.preserveQueryOnFocus || List.isEmpty selectedItems) then
                ( { model | query = Just "" }
                , Cmd.batch
                    [ cmd
                    , queryChangeCmd ""
                    ]
                )

            else
                ( model, cmd )

        OnBlur ->
            ( { model | query = Nothing }
            , case config.onBlur of
                Nothing ->
                    Cmd.none

                Just blurMessage ->
                    Task.succeed Nothing
                        |> Task.perform (\_ -> blurMessage)
            )

        OnClear ->
            let
                cmd : Cmd msg
                cmd =
                    Task.succeed Nothing
                        |> Task.perform config.onSelect
            in
            ( { model | query = Nothing }, cmd )

        OnRemoveItem item ->
            let
                cmd : Cmd msg
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
                transformedValue : String
                transformedValue =
                    value
                        |> config.transformInput

                transformedQuery : String
                transformedQuery =
                    transformedValue
                        |> config.transformQuery

                cmd : Cmd msg
                cmd =
                    if not (String.isEmpty transformedQuery) || config.emptySearch then
                        queryChangeCmd transformedQuery

                    else
                        Cmd.none
            in
            ( { model | highlightedItem = Nothing, query = Just transformedValue }, cmd )

        OnSelect item ->
            let
                cmd : Cmd msg
                cmd =
                    Task.succeed (Just item)
                        |> Task.perform config.onSelect
            in
            ( { model | query = Nothing }, cmd )
