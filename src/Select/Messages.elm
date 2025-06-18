module Select.Messages exposing (Msg(..))


type Msg item
    = NoOp
    | OnFocus (List item)
    | OnBlur
    | OnClear
    | OnRemoveItem item
    | OnEsc
    | OnDownArrow
    | OnUpArrow
    | OnQueryChange String
    | OnSelect item
