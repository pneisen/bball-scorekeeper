module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String


-- model


type alias Model =
    { players : List Player
    , playerName : String
    , playerId : Maybe Int
    , plays : List Play
    , currentPlayerId : Int
    , currentPlayId : Int
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , points : Int
    }


initialModel : Model
initialModel =
    { players = []
    , playerName = ""
    , playerId = Nothing
    , plays = []
    , currentPlayerId = 0
    , currentPlayId = 0
    }



-- update


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | Delete Play


update : Msg -> Model -> Model
update msg model =
    case msg of
        Edit player ->
            { model
                | playerId = Just player.id
                , playerName = player.name
            }

        Score player pts ->
            let
                id =
                    player.id
            in
                { model
                    | players =
                        List.map
                            (\player ->
                                if player.id == id then
                                    { player | points = player.points + pts }
                                else
                                    player
                            )
                            model.players
                    , plays = (Play model.currentPlayId id pts) :: model.plays
                    , currentPlayId = model.currentPlayId + 1
                }

        Input name ->
            { model | playerName = name }

        Cancel ->
            { model
                | playerName = ""
                , playerId = Nothing
            }

        Save ->
            if String.isEmpty model.playerName then
                model
            else
                save model

        Delete play ->
            { model
                | plays =
                    List.filter
                        (\p ->
                            if p.id /= play.id then
                                True
                            else
                                False
                        )
                        model.plays
                , players =
                    List.map
                        (\player ->
                            if play.playerId == player.id then
                                { player | points = player.points - play.points }
                            else
                                player
                        )
                        model.players
            }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit id model

        Nothing ->
            new model


new : Model -> Model
new model =
    let
        newPlayer =
            Player model.currentPlayerId model.playerName 0
    in
        { model
            | players = newPlayer :: model.players
            , playerName = ""
            , currentPlayerId = model.currentPlayerId + 1
        }


edit : Int -> Model -> Model
edit id model =
    { model
        | players =
            List.map
                (\player ->
                    if player.id == id then
                        { player | name = model.playerName }
                    else
                        player
                )
                model.players
        , playerName = ""
    }



-- view


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , playerSection model
        , playerForm model
        , playsSection model
        ]


playerSection : Model -> Html Msg
playerSection model =
    div []
        [ playerListHeader
        , playerList model
        , pointsTotal model
        ]


playerListHeader : Html Msg
playerListHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playerList : Model -> Html Msg
playerList model =
    ul [] (List.map player (List.sortBy .name model.players))


player : Player -> Html Msg
player player =
    li []
        [ i [ class "edit", onClick (Edit player) ] []
        , div [] [ text player.name ]
        , button [ type' "button", onClick (Score player 2) ] [ text "2 pts" ]
        , button [ type' "button", onClick (Score player 3) ] [ text "3 pts" ]
        , div [] [ text (toString player.points) ]
        ]


pointsTotal : Model -> Html Msg
pointsTotal model =
    let
        total =
            List.foldl (\player acc -> player.points + acc) 0 model.players
    in
        footer []
            [ div [] [ text "Total:" ]
            , div [] [ text (toString total) ]
            ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type' "text"
            , placeholder "Add/Edit Player..."
            , onInput Input
            , value model.playerName
            ]
            []
        , button [ type' "submit" ] [ text "Save" ]
        , button [ type' "button", onClick Cancel ] [ text "Cancel" ]
        ]


playsSection : Model -> Html Msg
playsSection model =
    div []
        [ playListHeader
        , playList model
        ]


playListHeader : Html Msg
playListHeader =
    header []
        [ div [] [ text "Plays" ]
        , div [] [ text "Points" ]
        ]


playList : Model -> Html Msg
playList model =
    model.plays |> List.map (play model) |> ul []


play : Model -> Play -> Html Msg
play model p =
    let
        playerMatch =
            List.filter
                (\player ->
                    if player.id == p.playerId then
                        True
                    else
                        False
                )
                model.players
                |> List.head

        playerName =
            case playerMatch of
                Just player ->
                    player.name

                Nothing ->
                    "Player not found"
    in
        li []
            [ i [ class "remove", onClick (Delete p) ] []
            , div [] [ text playerName ]
            , div [] [ text (toString p.points) ]
            ]



-- main


main : Program Never
main =
    App.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
