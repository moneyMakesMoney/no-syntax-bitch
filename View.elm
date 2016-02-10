module NoSyntaxBitch.View (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Signal exposing (Signal, Address)
import String
import Operators exposing (binary, unary)


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
        [ class "wrapper"
        , style [ ( "margin", "50px" ) ]
        ]
        [ section
            [ id "variables"
            , style
                [ ( "border-right", "1px solid #333" )
                , ( "width", "300px" )
                ]
            ]
            [ lazy2 newVariableEntry address model.field
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


newVariableEntry : Address Action -> String -> Html
newVariableEntry address varName =
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
                , name "newVar"
                , on "input" targetValue (Signal.message address << UpdateField)
                , onEnter address Add
                , style
                    [ ( "border", "none" )
                    , ( "outline", "none" )
                    ]
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


variableItemInput : Address Action -> Variable -> Html
variableItemInput address variable =
    input
        [ class "edit"
        , value variable.name
        , autofocus True
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
            [ p
                [ tabindex 1 ]
                [ text "const" ]
            , variableItemInput address variable
            , span [] [ text "=" ]
            , select
                []
                (List.map (\x -> option [] [ text x ]) binary)
              -- , button
              --     [ class "destroy"
              --     , tabindex -1
              --     , onClick address (Delete variable.id)
              --     ]
              --     [ text "×" ]
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
    emptyModel



-- Maybe.withDefault emptyModel getStorage
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
-- port getStorage : Maybe Model
-- port setStorage : Signal Model
-- port setStorage =
--     model
