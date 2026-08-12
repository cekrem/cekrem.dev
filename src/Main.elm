module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Process
import Task


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
    | Forkbombing Int (List Float)


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
            ( Forkbombing 0 [], Cmd.none )

        ( Forkbombing count prev, Fork f ) ->
            ( Forkbombing (count + 1) (f :: (prev |> List.take 128)), sendDelayedMsg (Fork <| f + 1) )

        ( Idle, Fork _ ) ->
            ( model, Cmd.none )

        ( Forkbombing _ _, Explode ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Idle ->
            Sub.none

        Forkbombing _ _ ->
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

        Forkbombing count entries ->
            Html.div
                [ Attr.style "position" "fixed"
                , Attr.style "z-index" "-1"
                , Attr.style "pointer-events" "none"
                ]
                (entries
                    |> List.map (viewFork count)
                )


sendDelayedMsg : msg -> Cmd msg
sendDelayedMsg msg =
    Process.sleep 1000
        |> Task.map (always msg)
        |> Task.perform identity


viewFork : Int -> Float -> Html msg
viewFork count f =
    Html.span
        [ Attr.style "margin" (String.fromFloat f ++ "rem")
        , Attr.style "font-size" (String.fromFloat f ++ "px")
        , Attr.style "opacity" (String.fromFloat (f / 10))
        ]
        [ Html.text <| "() -> " ++ String.fromInt count ]
