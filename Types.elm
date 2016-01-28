module NoSyntaxBitch where
import Html exposing (text)

type alias VariableName = String

type Node
  = Function { args: List Node, body: List Node, return: Node }
  | BinaryExpression { left: Node, right: Node, operator: String }
  | VariableDeclaration { identifier: Node, init: Node }
  | Constant Int
  | Variable VariableName
  | Nothing

fn : a -> b -> c -> { args : a, body : b, return : c }
fn args body return = { args = args, body = body, return = return }

add : { args : List Node, body : List Node, return : Node }
add = fn [Variable "a", Variable "b"]
      [ VariableDeclaration
          { identifier = Variable "sum"
          , init = BinaryExpression
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
