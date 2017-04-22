module Whiteboard.Queries exposing (..)

import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import GraphQL.Client.Http as GraphQLClient

import Whiteboard.Board exposing (Model as Board)

{- GraphQL schema:

type Card {
  board: Board @relation(name: "CardsOnBoard")
  content: String
  createdAt: DateTime!
  deletedAt: DateTime
  id: ID!
  updatedAt: DateTime!
  x: Int!
  y: Int!
}

type Board {
  cards: [Card!]! @relation(name: "CardsOnBoard")
  createdAt: DateTime!
  id: ID!
  updatedAt: DateTime!
}
-}

type alias Card =
  { content : String
  , x : Int
  , y : Int
  }

type alias Board =
  { cards: List Card
  , id: ID!
  , createdAt: DateTime!
  , updatedAt: DateTime!
  }
