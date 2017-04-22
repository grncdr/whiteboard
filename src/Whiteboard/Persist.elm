module Whiteboard.Persist exposing (..)

import Dict exposing (Dict)
import String
import Task
import Json.Encode as E
import Json.Decode as D exposing (field)
import Json.Helpers
import Whiteboard.Geometry exposing (Rect)
import Whiteboard.Board as Board
import Whiteboard.Card as Card


loadBoard_ =
    getStoredChanges
        |> Task.map (List.foldl Board.update (Board.init 256 128))


loadBoard =
    Task.succeed (maybeDecodeBoard Nothing)


record : Board.Model -> Board.Model -> Task.Task String ()
record old new =
    case diffBoards old new of
        [] ->
            -- no new changes -> no-op
            Task.succeed ()

        changes ->
            -- append to existing stored changes
            getStoredChanges
                |> Task.map (\changelog -> compactChanges (changelog ++ changes))
                |> Task.map (E.encode 0 << encodeBoardMessages)
                |> (flip Task.andThen) storeChanges


getStoredChanges =
    let
        decodeIfPresent s =
            Maybe.withDefault "[]" s
                |> D.decodeString decodeBoardMessages
                |> Task.fromResult
    in
        LocalStorage.get "Changes"
            |> Task.mapError storageErrorToString
            |> (flip Task.andThen) decodeIfPresent



-- Removes redundant AlterCard messages from the list


compactChanges : List Board.Msg -> List Board.Msg
compactChanges msgs =
    let
        replaceOp op msg cardDict =
            Dict.insert op msg (Maybe.withDefault Dict.empty cardDict) |> Just

        reducer msg dict =
            case msg of
                Board.AddCard id card ->
                    Dict.insert id (Dict.singleton "Add" msg) dict

                Board.RemoveCard id ->
                    case Maybe.map (Dict.get "AddCard") (Dict.get id dict) of
                        Just msg ->
                            -- card was created in this set of changes, just remove it
                            Dict.remove id dict

                        Nothing ->
                            -- card existed before this set of changes, add a remove message
                            Dict.insert id (Dict.singleton "RemoveCard" msg) dict

                Board.AlterCard id (Card.SetRect _) ->
                    Dict.update id (replaceOp "SetRect" msg) dict

                Board.AlterCard id (Card.SetColor _) ->
                    Dict.update id (replaceOp "SetColor" msg) dict

                Board.AlterCard id (Card.SetText _) ->
                    Dict.update id (replaceOp "SetText" msg) dict

                _ ->
                    dict
    in
        List.foldl reducer Dict.empty msgs
            |> Dict.toList
            |> List.sortBy (\( id, _ ) -> id)
            |> List.concatMap (\( id, ops ) -> Dict.toList ops |> List.sortBy (\( op, _ ) -> op))
            |> List.map (\( _, msg ) -> msg)


storeChanges s =
    LocalStorage.set "Changes" s |> Task.mapError storageErrorToString


decodeBoardMessages =
    D.list decodeBoardMsg


encodeBoardMessages : List Board.Msg -> E.Value
encodeBoardMessages =
    List.filterMap maybeEncodeBoardMsg >> E.list


decodeBoardMsg =
    Json.Helpers.decodeSumTwoElemArray "Board.Msg"
        (Dict.fromList
            [ ( "AlterCard"
              , D.map2 Board.AlterCard (field "id" D.string) ("msg" := decodeCardMsg)
              )
            , ( "AddCard"
              , D.map2 Board.AddCard (field "id" D.string) ("card" := decodeCard)
              )
            , ( "RemoveCard"
              , D.map Board.RemoveCard (field "id" D.string)
              )
            ]
        )


maybeEncodeBoardMsg msg =
    let
        pair =
            case msg of
                Board.AlterCard id msg ->
                    Maybe.map
                        (\msg -> ( "AlterCard", E.object [ ( "id", E.string id ), ( "msg", msg ) ] ))
                        (encodeCardMsg msg)

                Board.AddCard id card ->
                    Just ( "AddCard", E.object [ ( "id", E.string id ), ( "card", encodeCard card ) ] )

                Board.RemoveCard id ->
                    Just ( "RemoveCard", E.object [ ( "id", E.string id ) ] )

                _ ->
                    Nothing
    in
        Maybe.map (\( tag, value ) -> E.list [ E.string tag, value ]) pair


encodeCardMsg : Card.Msg -> Maybe E.Value
encodeCardMsg msg =
    case msg of
        Card.SetText text ->
            Just <| E.list [ E.string "SetText", E.string text ]

        Card.SetColor color ->
            Just <| E.list [ E.string "SetColor", E.string color ]

        Card.SetRect rect ->
            Just <| E.list [ E.string "SetRect", encodeRect rect ]

        _ ->
            Nothing


decodeCardMsg : D.Decoder Card.Msg
decodeCardMsg =
    Json.Helpers.decodeSumTwoElemArray "Card.Msg"
        (Dict.fromList
            [ ( "SetText", D.map Card.SetText D.string )
            , ( "SetColor", D.map Card.SetColor D.string )
            , ( "SetRect", D.map Card.SetRect decodeRect )
            ]
        )


encodeRect : Rect -> E.Value
encodeRect { x, y, w, h } =
    E.object
        [ ( "x", E.float x )
        , ( "y", E.float y )
        , ( "w", E.float w )
        , ( "h", E.float h )
        ]


decodeRect : D.Decoder Rect
decodeRect =
    D.map4 Rect
        (field "x" D.float)
        (field "y" D.float)
        (field "w" D.float)
        (field "h" D.float)


record_ old new =
    if equivalentCardDicts old.cards new.cards then
        Task.succeed ()
    else
        encodeBoard new
            |> E.encode 0
            |> Debug.log "set board"
            |> LocalStorage.set "Board"
            |> Task.mapError storageErrorToString


diffBoards : Board.Model -> Board.Model -> List Board.Msg
diffBoards old new =
    let
        changes ( id, new ) =
            case Dict.get id old.cards of
                Nothing ->
                    [ Board.AddCard id new ]

                Just old ->
                    diffCards old new |> List.map (Board.AlterCard id)

        deletions =
            Dict.keys old.cards
                |> List.filterMap
                    (\k ->
                        case Dict.get k new.cards of
                            Nothing ->
                                Just k

                            Just c ->
                                if c.deleted then
                                    Just k
                                else
                                    Nothing
                    )
                |> List.map Board.RemoveCard
    in
        deletions ++ (Dict.toList new.cards |> List.concatMap changes)


diffCards : Card.Model -> Card.Model -> List Card.Msg
diffCards old new =
    let
        setRect =
            if
                old.x
                    /= new.x
                    || old.y
                    /= new.y
                    || old.w
                    /= new.w
                    || old.h
                    /= new.h
            then
                [ Card.SetRect (Rect new.x new.y new.w new.h) ]
            else
                []

        setContent =
            if old.textContent /= new.textContent then
                [ Card.SetText new.textContent ]
            else
                []

        setColor =
            if old.color /= new.color then
                [ Card.SetColor new.color ]
            else
                []
    in
        setRect ++ setContent ++ setColor


equivalentCardDicts : Dict Board.Id Card.Model -> Dict Board.Id Card.Model -> Bool
equivalentCardDicts oldDict newDict =
    let
        equivalentCards new old =
            old.x
                == new.x
                && old.y
                == new.y
                && old.w
                == new.w
                && old.h
                == new.h
                && old.textContent
                == new.textContent
                && old.color
                == new.color
                && old.deleted
                == new.deleted

        cardUnchanged ( id, newCard ) =
            Dict.get id oldDict
                |> Maybe.map (equivalentCards newCard)
                |> Maybe.withDefault False
    in
        newDict |> Dict.toList |> List.all cardUnchanged


encodeBoard : Board.Model -> E.Value
encodeBoard { rows, cols, cards } =
    E.object
        [ ( "cols", E.int cols )
        , ( "rows", E.int rows )
        , ( "cards", E.list (Dict.toList cards |> List.filterMap encodeCardWithId) )
        ]


encodeCard : Card.Model -> E.Value
encodeCard { x, y, w, h, color, textContent, deleted } =
    if deleted then
        Debug.crash "Attempted to encode deleted card"
    else
        E.object
            [ ( "x", E.float x )
            , ( "y", E.float y )
            , ( "w", E.float w )
            , ( "h", E.float h )
            , ( "color", E.string color )
            , ( "textContent", E.string textContent )
            ]


encodeCardWithId : ( String, Card.Model ) -> Maybe E.Value
encodeCardWithId ( id, { x, y, w, h, color, textContent, deleted } ) =
    if deleted then
        Nothing
    else
        Just <|
            E.object
                [ ( "id", E.string id )
                , ( "x", E.float x )
                , ( "y", E.float y )
                , ( "w", E.float w )
                , ( "h", E.float h )
                , ( "color", E.string color )
                , ( "textContent", E.string textContent )
                ]


maybeDecodeBoard : Maybe String -> Result String Board.Model
maybeDecodeBoard s =
    Maybe.withDefault
        (Ok (Board.init 256 128))
        (Maybe.map (D.decodeString decodeBoard) s)


decodeBoard : D.Decoder Board.Model
decodeBoard =
    let
        init cols rows cards =
            Board.init cols rows
                |> \m -> { m | cards = cards }
    in
        D.map3 init
            (field "cols" D.int)
            (field "rows" D.int)
            (field "cards" (D.list decodeCard_ |> D.map Dict.fromList))


decodeCard : D.Decoder Card.Model
decodeCard =
    let
        init x y w h color textContent =
            { x = x
            , y = y
            , w = w
            , h = h
            , color = color
            , textContent = textContent
            , mouse = { x = 0, y = 0 }
            , dragStart = { x = 0, y = 0 }
            , drag = Nothing
            , showSettings = False
            , deleted = False
            }
    in
        D.map6 init
            (field "x" D.float)
            (field "y" D.float)
            (field "w" D.float)
            (field "h" D.float)
            (field "color" D.string)
            (field "textContent" D.string)


decodeCard_ : D.Decoder ( Board.Id, Card.Model )
decodeCard_ =
    let
        init id x y w h color textContent =
            ( id
            , { x = x
              , y = y
              , w = w
              , h = h
              , color = color
              , textContent = textContent
              , mouse = { x = 0, y = 0 }
              , dragStart = { x = 0, y = 0 }
              , drag = Nothing
              , showSettings = False
              , deleted = False
              }
            )
    in
        D.map7 init
            (field "id" (D.oneOf [ D.string, (D.map toString D.int) ]))
            (field "x" D.float)
            (field "y" D.float)
            (field "w" D.float)
            (field "h" D.float)
            ("color" D.string)
            (field "textContent" D.string)

