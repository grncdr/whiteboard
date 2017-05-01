module Whiteboard.Backend exposing (..)

import Http
import GraphCool
import Task exposing (Task)
import ISO8601
import Json.Decode as Decode
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Client.Http as GraphQLClient

type alias ClientError = GraphQLClient.Error

mutate : Maybe Authorization -> Document Mutation res params -> params -> Task ClientError res
mutate auth doc params =
  request params doc |> GraphQLClient.customSendMutation (opts auth)

query : Maybe Authorization -> Document Query res params -> params -> Task ClientError res
query auth doc params =
  request params doc |> GraphQLClient.customSendQuery (opts auth)

opts : Maybe Authorization -> GraphQLClient.RequestOptions
opts auth =
    { method = "POST"
    , url = "https://api.graph.cool/simple/v1/cj1t5q0el8t3s0109xy3shmyh"
    , timeout = Nothing
    , withCredentials = False
    , headers = case auth of
        Nothing -> []
        Just a -> [ Http.header "Authorization" ("Bearer " ++ a.token) ]
    }

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

type alias Authorization =
    { token : String
    , user : String
    }

authorization : ValueSpec NonNull ObjectType Authorization vars
authorization =
    object Authorization
        |> with (field "token" [] string)
        |> with (field "user"  [] (extract (field "id" [] id)))

createUser : Document Mutation String { vars | email : String , password : String }
createUser =
    let
        emailVar =
            Var.required "email" .email Var.string

        passwordVar =
            Var.required "password" .password Var.string
    in
        mutationDocument <|
            extract
                (field "createUser"
                    [ ( "authProvider",
                        Arg.object
                          [ ( "email"
                            , Arg.object
                                [ ( "email", Arg.variable emailVar )
                                , ( "password", Arg.variable passwordVar )
                                ]
                            )
                          ]
                       )
                    ]
                    (extract (field "id" [] string))
                )

signinUser : Document Mutation Authorization { vars | email : String , password : String }
signinUser =
    let
        emailVar =
            Var.required "email" .email Var.string

        passwordVar =
            Var.required "password" .password Var.string
    in
        mutationDocument <|
            extract
                (field "signinUser"
                    [ ( "email"
                      , Arg.object
                          [ ( "email", Arg.variable emailVar )
                          , ( "password", Arg.variable passwordVar ) ] )
                    ]
                    authorization
                )

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


loadBoards : Document Query (List Board) { vars | user : String }
loadBoards =
    let
        userId1 = Var.required "userId1" .user Var.id
        userId2 = Var.required "userId2" .user Var.id
        filter = GraphCool.or
          [ GraphCool.eq "owner" <| GraphCool.eq "id" (Arg.variable userId2)
          , GraphCool.some "members" <| GraphCool.eq "id" (Arg.variable userId1)
          ]
    in
       queryDocument <|
         extract (field "allBoards" [ ( "filter", filter ) ] (list board))

createBoard : Document Mutation Board { vars | user : String }
createBoard =
  let
      ownerId = Var.required "ownerId" .user Var.id
  in
      mutationDocument <|
        extract (field "createBoard" [ ( "ownerId", Arg.variable ownerId ) ] board)

loadBoard : Document Query Board { vars | boardID : String }
loadBoard =
    let
        boardIDVar =
            Var.required "boardID" .boardID Var.id
        queryRoot =
            extract (field "Board" [ ( "id", Arg.variable boardIDVar ) ] board)
    in
        queryDocument queryRoot

type alias CreateCardParams vars =
    { vars
    | boardId : String
    , textContent : String
    , x : Float
    , y : Float
    , w : Float
    , h : Float
    , color : String
    }

createCard : String -> Document Mutation Card (CreateCardParams v)
createCard creatorId =
    let
        boardId =
            Var.required "boardId" .boardId Var.id

        width =
            Var.required "w" .w Var.float

        height =
            Var.required "h" .h Var.float

        x =
            Var.required "x" .x Var.float

        y =
            Var.required "y" .y Var.float

        color =
            Var.required "color" .color Var.string

        content =
            Var.required "content" .textContent Var.string

        args =
            [ ( "boardId", Arg.variable boardId )
            , ( "creatorId", Arg.string creatorId )
            , ( "w", Arg.variable width )
            , ( "h", Arg.variable height )
            , ( "x", Arg.variable x )
            , ( "y", Arg.variable y )
            , ( "color", Arg.variable color )
            , ( "content", Arg.variable content )
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
