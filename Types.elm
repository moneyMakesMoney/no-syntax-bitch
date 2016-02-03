module NoSyntaxBitch.Types (..) where

import Html exposing (text)


type alias VariableName =
    String


type alias OperatorName =
    String


type VariableDeclaration
    = Declaration
        { type' : String
        , id : { type' : String, name : String }
        , init : Node
        }


type Node
    = Function
        { params : List Node
        , defaults : List Node
        , body : List Node
        , generator : Bool
        , expression : Bool
        , return : Node
        }
    | BinaryExpression { left : Node, right : Node, operator : OperatorName }
    | UnaryExpression { right : Node, operator : OperatorName }
    | VariableDeclaration { identifier : Node, init : Node }
    | Constant Int
    | Variable VariableName
    | VariableDeclarations { declarations : List VariableDeclaration, kind : String }
    | Nothing


fn : a -> b -> c -> { params : a, body : b, return : c }
fn params body return =
    { params = params, body = body, return = return }


add : { params : List Node, body : List Node, return : Node }
add =
    fn
        [ Variable "a", Variable "b" ]
        [ VariableDeclaration
            { identifier = Variable "sum"
            , init =
                BinaryExpression
                    { left = Variable "a"
                    , right = Variable "b"
                    , operator = "+"
                    }
            }
        , VariableDeclaration { identifier = Variable "number", init = Constant 56 }
        ]
        (Variable "sum")


main : Html.Html
main =
    text "Hello, World!"
