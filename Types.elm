module NoSyntaxBitch where
import Html exposing (text)

type alias VariableName = String

type Node
  = Function { args: List Node, body: List Node, return: Node }
  | BinaryExpression { left: Node, right: Node, operator: String }
  | VariableDeclaration { identifier: Node, init: Node }
  | Constant Int
  | VariableName VariableName
  | Nothing

fn : a -> b -> c -> { args : a, body : b, return : c }
fn args body return = { args = args, body = body, return = return }

add : { args : List Node, body : List Node, return : Node }
add = fn [VariableName "a", VariableName "b"]
      [ VariableDeclaration
          { identifier = VariableName "sum"
          , init = BinaryExpression
                   { left = VariableName "a"
                   , right = VariableName "b"
                   , operator = "+"
                   }
          }
      , VariableDeclaration { identifier = VariableName "number", init = Constant 56 }
      ]
      (VariableName "sum")

main : Html.Html
main =
  text "Hello, World!"
