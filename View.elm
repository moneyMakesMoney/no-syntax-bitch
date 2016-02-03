module Todo (..) where

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.
This application is broken up into four distinct parts:
  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML
  4. Inputs - the signals necessary to manage events
This clean division of concerns is a core part of Elm. You can read more about
this in the Pong tutorial: http://elm-lang.org/blog/making-pong
This program is not particularly large, so definitely see the following
for notes on structuring more complex GUIs with Elm:
https://github.com/evancz/elm-architecture-tutorial/
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Signal exposing (Signal, Address)
import String


---- MODEL ----
-- The full application state of our app.


type alias Model =
    { variables : List Variable
    , field : String
    , uid : Int
    , visibility : String
    }


type alias Variable =
    { name : String
    , editing : Bool
    , id : Int
    }


newVariable : String -> Int -> Variable
newVariable name id =
    { name = name
    , editing = False
    , id = id
    }


emptyModel : Model
emptyModel =
    { variables = []
    , visibility = "All"
    , field = ""
    , uid = 0
    }



---- UPDATE ----
-- A name of the kinds of actions that can be performed on the model of
-- our application. See the following for more info on this pattern and
-- some alternatives: https://github.com/evancz/elm-architecture-tutorial/


type Action
    = NoOp
    | UpdateField String
    | EditingVariable Int Bool
    | UpdateVariable Int String
    | Add
    | Delete Int



-- How we update our Model on a given Action?


update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        Add ->
            { model
                | uid = model.uid + 1
                , field = ""
                , variables =
                    if String.isEmpty model.field then
                        model.variables
                    else
                        model.variables ++ [ newVariable model.field model.uid ]
            }

        UpdateField str ->
            { model | field = str }

        EditingVariable id isEditing ->
            let
                updateVariable t =
                    if t.id == id then
                        { t | editing = isEditing }
                    else
                        t
            in
                { model | variables = List.map updateVariable model.variables }

        UpdateVariable id name ->
            let
                updateVariable t =
                    if t.id == id then
                        { t | name = name }
                    else
                        t
            in
                { model | variables = List.map updateVariable model.variables }

        Delete id ->
            { model | variables = List.filter (\t -> t.id /= id) model.variables }



---- VIEW ----


view : Address Action -> Model -> Html
view address model =
    div
        [ class "todomvc-wrapper"
        , style [ ( "visibility", "hidden" ) ]
        ]
        [ section
            [ id "todoapp" ]
            [ lazy2 variableEntry address model.field
            , lazy3 variablesList address model.visibility model.variables
            , lazy3 controls address model.visibility model.variables
            ]
        ]


onEnter : Address a -> a -> Attribute
onEnter address value =
    on
        "keydown"
        (Json.customDecoder keyCode is13)
        (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
    if code == 13 then
        Ok ()
    else
        Err "not the right key code"


variableEntry : Address Action -> String -> Html
variableEntry address varName =
    header
        [ id "header" ]
        [ h1 [] [ text "variables" ]
        , div
            [ class "new-variable-container" ]
            [ span [] [ text "const " ]
            , input
                [ id "new-variable"
                , placeholder "variable"
                , autofocus True
                , value varName
                , name "newTodo"
                , on "input" targetValue (Signal.message address << UpdateField)
                , onEnter address Add
                ]
                []
            ]
        ]


variablesList : Address Action -> String -> List Variable -> Html
variablesList address visibility variables =
    section
        [ id "main"
        ]
        [ ul
            [ id "variables-list" ]
            (List.map (variableItem address) variables)
        ]


variableItemShow : Address Action -> Variable -> Html
variableItemShow address variable =
    span
        []
        [ label
            [ onDoubleClick address (EditingVariable variable.id True) ]
            [ text variable.name ]
        , button
            [ class "destroy"
            , onClick address (Delete variable.id)
            ]
            []
        ]


variableItemInput : Address Action -> Variable -> Html
variableItemInput address variable =
    input
        [ class "edit"
        , value variable.name
        , name "title"
        , id ("variable-" ++ toString variable.id)
        , on "input" targetValue (Signal.message address << UpdateVariable variable.id)
        , onBlur address (EditingVariable variable.id False)
        , onEnter address (EditingVariable variable.id False)
        ]
        []


variableItem : Address Action -> Variable -> Html
variableItem address variable =
    li
        [ classList [ ( "editing", variable.editing ) ] ]
        [ div
            [ class "view" ]
            [ p [] [ text "const" ]
            , if variable.editing then
                variableItemInput address variable
              else
                variableItemShow address variable
            ]
        ]


controls : Address Action -> String -> List Variable -> Html
controls address visibility variables =
    let
        varsNumber = List.length variables

        item_ =
            if varsNumber == 1 then
                " variable"
            else
                " variables"
    in
        footer
            [ id "footer"
            ]
            [ span
                [ id "todo-count" ]
                [ span [] [ text (toString varsNumber) ]
                , text item_
                ]
            ]



---- INPUTS ----
-- wire the entire application together


main : Signal Html
main =
    Signal.map (view actions.address) model



-- manage the model of our application over time


model : Signal Model
model =
    Signal.foldp update initialModel actions.signal


initialModel : Model
initialModel =
    Maybe.withDefault emptyModel getStorage



-- actions from user input


actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp


port focus : Signal String
port focus =
    let
        needsFocus act =
            case act of
                EditingVariable id bool ->
                    bool

                _ ->
                    False

        toSelector act =
            case act of
                EditingVariable id _ ->
                    "#todo-" ++ toString id

                _ ->
                    ""
    in
        actions.signal
            |> Signal.filter needsFocus (EditingVariable 0 True)
            |> Signal.map toSelector



-- interactions with localStorage to save the model


port getStorage : Maybe Model
port setStorage : Signal Model
port setStorage =
    model
