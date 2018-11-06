port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Json.Decode as JD



-- MAIN


main =
  Browser.element { init = init, update = updateWithStorage, subscriptions = subscriptions, view = view }


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )


---- PORT ----


port setStorage : Model -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { items : List TodoItem
    , uid: Int
    , newTodoContent: String
    , errorVisibility: Bool
    }


type alias TodoItem = 
    { id : Int
    , content : String
    , completed : Bool
    }


newItem : String -> Int -> TodoItem
newItem desc id =
    { id = id
    , content = desc
    , completed = False
    }


init : Maybe Model -> ( Model, Cmd Msg )
init model = 
    ( Maybe.withDefault (Model [] 0 "" False) model, Cmd.none )



-- UPDATE


type Msg
    = UpdateNewTodo String
    | Add
    | Modify Int String
    | Delete Int
    | Complete Int Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Add ->
        (
          if model.newTodoContent /= "" then
              { model | items =
                  model.items ++ [ newItem model.newTodoContent model.uid ]
              , uid = model.uid + 1
              , newTodoContent = ""
              , errorVisibility = False 
              }
          else
              { model | errorVisibility = True }
        , Cmd.none
        )

    Modify id content ->
        let
            updateEntry item =
                if item.id == id then
                    { item | content = content }
                else
                    item
        in
        ( { model | items = List.map updateEntry model.items }
        , Cmd.none
        )

    Delete id ->
        ( { model | items = List.filter (\t -> t.id /= id) model.items }
        , Cmd.none
        )

    UpdateNewTodo text ->
        ( { model | newTodoContent = text }
        , Cmd.none
        )
        
    Complete id isCompleted ->
        let
            updateEntry item =
                if item.id == id then
                    { item | completed = isCompleted }
                else
                    item
        in
        ( { model | items = List.map updateEntry model.items }
        , Cmd.none
        )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ div
                [] [ viewInput "text" "Add a new todo" model.newTodoContent UpdateNewTodo Add ]
                , viewErrorMessage model.errorVisibility
                , viewTodoItems model.items
            ]
        ]


viewInput : String -> String -> String -> (String -> Msg) -> Msg -> Html Msg
viewInput t p v inputMsg enterMsg =
    input [ type_ t
          , placeholder p
          , value v
          , autofocus True
          , onInput inputMsg
          , onEnter enterMsg
          ] []


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                JD.succeed msg
            else
                JD.fail "not ENTER"
    in
        on "keydown" (JD.andThen isEnter keyCode)


viewTodoItems : List TodoItem -> Html Msg
viewTodoItems todoitems =
    ul []
        (List.map viewTodoItem todoitems)


viewTodoItem : TodoItem -> Html Msg
viewTodoItem todo =
    li []
        [ viewCompleteCheckBox todo
        , textarea [] [ text todo.content ]
        , viewDeleteButton todo.id
        ]


viewCompleteCheckBox : TodoItem -> Html Msg
viewCompleteCheckBox todo =
    div []
        [ input
            [ type_ "checkbox"
            , checked todo.completed
            , onClick (Complete todo.id (not todo.completed))
            ] []
        ]


viewDeleteButton : Int -> Html Msg
viewDeleteButton id =
    button [ onClick (Delete id) ] [ text "close" ]


viewErrorMessage : Bool -> Html Msg
viewErrorMessage visibility =
    if visibility then
        div [] [ text "Please enter something!" ]
    else
        Html.text ""