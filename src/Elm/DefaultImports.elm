module Elm.DefaultImports exposing (defaults, operatorTable)

import Dict exposing (Dict)
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range


defaults : List Import
defaults =
    [ { moduleName = Node Range.emptyRange <| [ "Basics" ]
      , exposingList = Just (Node Range.emptyRange <| All Range.emptyRange)
      , moduleAlias = Nothing
      }
    , { moduleName = Node Range.emptyRange <| [ "List" ]
      , exposingList =
            Just <|
                Node Range.emptyRange <|
                    Explicit
                        [ Node Range.emptyRange <| TypeExpose (ExposedType "List" Nothing)
                        , Node Range.emptyRange <| InfixExpose "::"
                        ]
      , moduleAlias = Nothing
      }
    , { moduleName = Node Range.emptyRange <| [ "Maybe" ]
      , exposingList =
            Just <|
                Node Range.emptyRange <|
                    Explicit
                        [ Node Range.emptyRange <|
                            TypeExpose
                                (ExposedType "Maybe" (Just Range.emptyRange))
                        ]
      , moduleAlias = Nothing
      }
    , { moduleName = Node Range.emptyRange <| [ "Result" ]
      , exposingList =
            Just <|
                Node Range.emptyRange <|
                    Explicit
                        [ Node Range.emptyRange <|
                            TypeExpose
                                (ExposedType "Result" (Just Range.emptyRange))
                        ]
      , moduleAlias = Nothing
      }
    , { moduleName = Node Range.emptyRange <| [ "String" ], exposingList = Nothing, moduleAlias = Nothing }
    , { moduleName = Node Range.emptyRange <| [ "Tuple" ], exposingList = Nothing, moduleAlias = Nothing }
    , { moduleName = Node Range.emptyRange <| [ "Debug" ], exposingList = Nothing, moduleAlias = Nothing }
    , { moduleName = Node Range.emptyRange <| [ "Platform" ]
      , exposingList =
            Just <|
                Node Range.emptyRange <|
                    Explicit
                        [ Node Range.emptyRange <| TypeExpose (ExposedType "Program" Nothing)
                        ]
      , moduleAlias = Nothing
      }
    , { moduleName = Node Range.emptyRange <| [ "Platform", "Cmd" ]
      , exposingList =
            Just <|
                Node Range.emptyRange <|
                    Explicit
                        [ Node Range.emptyRange <| TypeExpose (ExposedType "Cmd" Nothing)
                        , Node Range.emptyRange <| InfixExpose "!"
                        ]
      , moduleAlias = Nothing
      }
    , { moduleName = Node Range.emptyRange <| [ "Platform", "Sub" ]
      , exposingList = Just <| Node Range.emptyRange <| Explicit [ Node Range.emptyRange <| TypeExpose (ExposedType "Sub" Nothing) ]
      , moduleAlias = Nothing
      }
    ]


operatorTable : Dict String Infix
operatorTable =
    let
        infix direction precedence operator function =
            { direction = Node Range.emptyRange direction
            , precedence = Node Range.emptyRange precedence
            , operator = Node Range.emptyRange operator
            , function = Node Range.emptyRange function
            }
    in
    [ -- https://github.com/elm/core/blob/84f38891468e8e153fc85a9b63bdafd81b24664e/src/Basics.elm#L71-L89
      infix Right 0 "<|" "apL"
    , infix Left 0 "|>" "apR"
    , infix Right 2 "||" "or"
    , infix Right 3 "&&" "and"
    , infix Non 4 "==" "eq"
    , infix Non 4 "/=" "neq"
    , infix Non 4 "<" "lt"
    , infix Non 4 ">" "gt"
    , infix Non 4 "<=" "le"
    , infix Non 4 ">=" "ge"
    , infix Right 5 "++" "append"
    , infix Left 6 "+" "add"
    , infix Left 6 "-" "sub"
    , infix Left 7 "*" "mul"
    , infix Left 7 "/" "fdiv"
    , infix Left 7 "//" "idiv"
    , infix Right 8 "^" "pow"
    , infix Left 9 "<<" "composeL"
    , infix Right 9 ">>" "composeR"

    -- https://github.com/elm/parser/blob/02839df10e462d8423c91917271f4b6f8d2f284d/src/Parser.elm#L58-L59
    , infix Left 5 "|=" "keeper"
    , infix Left 6 "|." "ignorer"

    -- https://github.com/elm/core/blob/84f38891468e8e153fc85a9b63bdafd81b24664e/src/List.elm#L41
    , infix Right 5 "::" "cons"
    ]
        |> List.map (\i -> ( Node.value i.operator, i ))
        |> Dict.fromList
