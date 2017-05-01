module Whiteboard.Backend exposing (..)

import Task exposing (Task)
import ISO8601
import Json.Decode as Decode
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Client.Http as GraphQLClient

type alias ClientError = GraphQLClient.Error

endpoint =
        "https://api.graph.cool/simple/v1/cj1t5q0el8t3s0109xy3shmyh"

mutate : Document Mutation res params -> params -> Task ClientError res
mutate doc params =
  request params doc |> GraphQLClient.sendMutation endpoint

query : Document Query res params -> params -> Task ClientError res
query doc params =
  request params doc |> GraphQLClient.sendQuery endpoint

type DateTimeType
    = DateTimeType

datetime : ValueSpec NonNull DateTimeType ISO8601.Time vars
datetime =
    Decode.string
        |> Decode.andThen
            (\timeString ->
                case ISO8601.fromString timeString of
                    Ok time ->
                        Decode.succeed time
                    Err errorMessage ->
                        Decode.fail errorMessage)
        |> customScalar DateTimeType

type alias Board =
    { id : String
    , w : Int
    , h : Int
    , cards : List Card
    , createdAt : ISO8601.Time
    , updatedAt : ISO8601.Time
    }


board : ValueSpec NonNull ObjectType Board vars
board =
    object Board
        |> with (field "id" [] id)
        |> with (field "w" [] int)
        |> with (field "h" [] int)
        |> with (field "cards" [] (list card))
        |> with (field "createdAt" [] datetime)
        |> with (field "updatedAt" [] datetime)


type alias Card =
    { id : String
    , content : String
    , x : Float
    , y : Float
    , w : Float
    , h : Float
    , color : String
    , createdAt : ISO8601.Time
    , updatedAt : ISO8601.Time
    , deleted : Bool
    }


card : ValueSpec NonNull ObjectType Card vars
card =
    object Card
        |> with (field "id" [] id)
        |> with (field "content" [] string)
        |> with (field "x" [] float)
        |> with (field "y" [] float)
        |> with (field "w" [] float)
        |> with (field "h" [] float)
        |> with (field "color" [] string)
        |> with (field "createdAt" [] datetime)
        |> with (field "updatedAt" [] datetime)
        |> with (field "deleted" [] bool)


loadBoard : Document Query Board { vars | boardID : String }
loadBoard =
    let
        boardIDVar =
            Var.required "boardID" .boardID Var.id
        queryRoot =
            extract (field "Board" [ ( "id", Arg.variable boardIDVar ) ] board)
    in
        queryDocument queryRoot

createCard : Document Mutation Card { vars
                                    | boardId : String
                                    , textContent : String
                                    , x : Float
                                    , y : Float
                                    , w : Float
                                    , h : Float
                                    , color : String
                                    }
createCard =
    let
        boardIDVar =
            Var.required "boardId" .boardId Var.id

        widthVar =
            Var.required "w" .w Var.float

        heightVar =
            Var.required "h" .h Var.float

        xVar =
            Var.required "x" .x Var.float

        yVar =
            Var.required "y" .y Var.float

        colorVar =
            Var.required "color" .color Var.string

        contentVar =
            Var.required "content" .textContent Var.string

        args =
            [ ( "boardId", Arg.variable boardIDVar )
            , ( "w", Arg.variable widthVar )
            , ( "h", Arg.variable heightVar )
            , ( "x", Arg.variable xVar )
            , ( "y", Arg.variable yVar )
            , ( "color", Arg.variable colorVar )
            , ( "content", Arg.variable contentVar )
            ]
    in
        extract (field "createCard" args card) |> mutationDocument

simpleArg name fn ty =
  (name, Arg.variable (Var.required name fn ty))

idArg = simpleArg "id" .id Var.id

cardUpdateMutation args =
   field "updateCard" (idArg :: args) card
   |> extract
   |> mutationDocument


moveCard : Document Mutation Card { vars | id : String, x : Float, y : Float }
moveCard =
  cardUpdateMutation
    [ simpleArg "x" .x Var.float
    , simpleArg "y" .y Var.float
    ]


resizeCard : Document Mutation Card { vars | id : String, w : Float, h : Float }
resizeCard =
  cardUpdateMutation
    [ simpleArg "w" .w Var.float
    , simpleArg "h" .h Var.float
    ]

setCardContent : Document Mutation Card { vars | id : String, w : Float, h : Float, textContent : String }
setCardContent =
  cardUpdateMutation
    [ simpleArg "w" .w Var.float
    , simpleArg "h" .h Var.float
    , simpleArg "content" .textContent Var.string
    ]

setCardColor : Document Mutation Card { vars | id : String, color : String }
setCardColor =
  cardUpdateMutation [ simpleArg "color" .color Var.string ]

setCardDeleted : Document Mutation Card { vars | id : String, deleted : Bool }
setCardDeleted =
  cardUpdateMutation [ simpleArg "deleted" .deleted Var.bool ]
{-
   createOrUpdateCard : Card -> Task GraphQLClient.Error Card
   createOrUpdateCard card =
     if card.id == ""
       boardQuery |> request { boardID = boardID } |> sendQuery
-}
