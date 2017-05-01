module Whiteboard.Login exposing (..)

import Task
import Html exposing (Html, text, form, input, button, label, div)
import Html.Attributes exposing (value, checked, style, disabled, type_)
import Html.Events exposing (..)
import Whiteboard.Backend as Backend


type alias Model =
    { email : String
    , password : String
    , passwordConfirmation : String
    , isSignup : Bool
    , submitting : Bool
    , error : Maybe Backend.ClientError
    , auth : Maybe Backend.Authorization
    }


type Msg
    = SetEmail String
    | SetPassword String
    | SetPasswordConfirmation String
    | SetIsSignup Bool
    | Submit
    | Response (Result Backend.ClientError Backend.Authorization)


init : Model
init =
    { email = ""
    , password = ""
    , passwordConfirmation = ""
    , isSignup = False
    , submitting = False
    , error = Nothing
    , auth = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        SetPasswordConfirmation password ->
            ( { model | passwordConfirmation = password }, Cmd.none )

        SetIsSignup isSignup ->
            ( { model | isSignup = isSignup }, Cmd.none )

        Submit ->
            ( { model | submitting = True, error = Nothing }
            , loginTask model |> Task.attempt Response
            )

        Response (Ok auth) ->
            ( { model | submitting = False, auth = Just auth }, Cmd.none )

        Response (Err error) ->
            ( { model | submitting = False, error = Just error }, Cmd.none )


loginTask : Model -> Task.Task Backend.ClientError Backend.Authorization
loginTask model =
    if model.isSignup then
        Backend.mutate Nothing Backend.createUser model
            |> Task.andThen (\id -> Backend.mutate Nothing Backend.signinUser model)
    else
        Backend.mutate Nothing Backend.signinUser model


view : Model -> Html Msg
view { email, password, passwordConfirmation, isSignup, submitting, error } =
    let
        disable =
            disabled submitting
    in
        form
            [ onSubmit Submit
            , style
                [ ( "margin", "5em auto" )
                , ( "max-width", "20em" )
                , ( "display", "flex" )
                , ( "flex-direction", "column" )
                ]
            ]
            [ formRow "E-mail: "
                [ input [ onInput SetEmail, value email, disable ] [] ]
            , formRow "Password: "
                [ input
                    [ type_ "password"
                    , onInput SetPassword
                    , value password
                    , disable
                    ]
                    []
                ]
            , formRow "Create a new account: "
                [ input
                    [ type_ "checkbox"
                    , onCheck SetIsSignup
                    , checked isSignup
                    , disable
                    ]
                    []
                ]
            , if isSignup then
                formRow "Confirm password: "
                    [ input
                        [ type_ "password"
                        , onInput SetPasswordConfirmation
                        , value passwordConfirmation
                        , disable
                        ]
                        []
                    ]
              else
                div [] []
            , formRow ""
                [ button [ disable ]
                    [ text <|
                        if isSignup then
                            "Create account"
                        else
                            "Sign in"
                    ]
                ]
            , case error of
                Nothing ->
                    div [] []

                Just err ->
                    div [ style [ ( "color", "red" ) ] ] [ err |> toString |> text ]
            ]


formRow hint children =
    label [ style ([ ( "display", "flex" ), ( "justify-content", "space-between" ), ( "margin", "0.5em 0" ) ]) ]
        ((text hint) :: children)
