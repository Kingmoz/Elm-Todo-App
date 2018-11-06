port module Main exposing (..)

import Browser
import Browser.Dom exposing (focus, blur)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Json.Decode as JD
import Svg as S
import Svg.Attributes as SA
import Task


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
    }


type alias TodoItem = 
    { id : Int
    , content : String
    , completed : Bool
    }


newItem : Int -> TodoItem
newItem id =
    { id = id
    , content = ""
    , completed = False
    }


init : Maybe Model -> ( Model, Cmd Msg )
init model = 
    ( Maybe.withDefault ( Model [] 0 ) model, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | Add
    | Modify Int String
    | Delete Int
    | Complete Int Bool
    | Blur Int


focusInput : Int -> Cmd Msg
focusInput id =
  Task.attempt (\_ -> NoOp) (focus ("item-" ++ String.fromInt id))


blurInput : Int -> Cmd Msg
blurInput id =
  Task.attempt (\_ -> NoOp) (blur ("item-" ++ String.fromInt id))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
        ( model, Cmd.none )
    
    Add ->
        ( { model | items =
            model.items ++ [ newItem model.uid ]
          , uid = model.uid + 1
          }
        , focusInput model.uid
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

    Blur id ->
        ( model , blurInput id )

-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "mdc-card card" ]
            [ div
                [ class "card__header" ]
                [ div
                    [ class "card__header__inner" ]
                    [ span
                        [ class "card__header__title" ]
                        [ text "Todo List" ]
                    , button
                        [ class "mdc-icon-button material-icons"
                        , onClick Add
                        ] [ text "add" ]
                    ]
                ]
            , viewTodoItems model.items
            ]
        ]


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
    ul [ class "mdc-list list" ]
        (List.map viewTodoItem todoitems)


viewTodoItem : TodoItem -> Html Msg
viewTodoItem todo =
    li [ class "mdc-list-item list__item" ]
        [ viewCompleteCheckBox todo
        , div
            [ class "mdc-list-item__text list__item__input" ]
            [ input 
                [ id ("item-" ++ String.fromInt todo.id)
                , class "list__item__input__text-area"
                , classList [ ( "completed", todo.completed ) ]
                , rows 1
                , value todo.content
                , onInput (Modify todo.id)
                , onEnter (Blur todo.id)
                ]
                []
            ]
        , viewDeleteButton todo.id
        ]


-- From: https://material.io/develop/web/components/input-controls/checkboxes/
viewCompleteCheckBox : TodoItem -> Html Msg
viewCompleteCheckBox todo =
    div [ class "mdc-checkbox list__item__complete" ]
        [ input
          [ class "mdc-checkbox__native-control"
          , type_ "checkbox"
          , checked todo.completed
          , onClick (Complete todo.id (not todo.completed))
          ] []
        , div [ class "mdc-checkbox__background" ]
            [
              S.svg [ SA.class "mdc-checkbox__checkmark", SA.viewBox "0 0 24 24" ]
                [
                  S.path
                    [ SA.class "mdc-checkbox__checkmark-path"
                    , SA.fill "none"
                    , SA.d "M1.73,12.91 8.1,19.28 22.79,4.59"
                    ] []
                ]
            ]
        , div [ class "mdc-checkbox__mixedmark" ] []
        ]


viewDeleteButton : Int -> Html Msg
viewDeleteButton id =
    button
        [ class "mdc-icon-button material-icons list__item__delete-btn"
        , onClick (Delete id)
        ]
        [ text "close" ]


viewErrorMessage : Bool -> Html Msg
viewErrorMessage visibility =
    if visibility then
        div [] [ text "Please enter something!" ]
    else
        Html.text ""