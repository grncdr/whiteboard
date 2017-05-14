module GraphCool exposing (..)
import GraphQL.Request.Builder.Arg as Arg

or predicates =
  Arg.object [ ( "OR", Arg.list predicates ) ]

and predicates =
  Arg.object [ ( "AND", Arg.list predicates ) ]

eq name value =
  Arg.object [ ( name , value ) ]

some name filter =
  Arg.object [ ( name ++ "_some" , filter ) ]
