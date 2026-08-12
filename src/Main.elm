module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Idle
    | Forkbombing (List Float)


init : ( Model, Cmd msg )
init =
    ( Idle, Cmd.none )


type Msg
    = Explode
    | Fork Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Idle, Explode ) ->
            ( Forkbombing [], Cmd.none )

        ( Forkbombing prev, Fork f ) ->
            ( Forkbombing (f :: (prev |> List.take 128)), Cmd.none )

        ( Idle, Fork _ ) ->
            ( model, Cmd.none )

        ( Forkbombing _, Explode ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Idle ->
            Sub.none

        Forkbombing _ ->
            Browser.Events.onAnimationFrameDelta Fork


view : Model -> Html Msg
view model =
    case model of
        Idle ->
            Html.code
                [ Attr.style "position" "fixed"
                , Attr.style "bottom" "0"
                , Attr.style "right" "0"
                , Attr.style "cursor" "not-allowed"
                , Events.onClick Explode
                ]
                [ Html.text ":(){ :|:& };:" ]

        Forkbombing entries ->
            Html.div
                [ Attr.style "position" "fixed"
                , Attr.style "z-index" "-1"
                , Attr.style "pointer-events" "none"
                ]
                (entries
                    |> List.map viewFork
                )


viewFork : Float -> Html msg
viewFork f =
    Html.span
        [ Attr.style "margin" (String.fromFloat f ++ "rem")
        , Attr.style "font-size" (String.fromFloat f ++ "px")
        , Attr.style "opacity" (String.fromFloat (f / 10))
        ]
        [ Html.text "()" ]
