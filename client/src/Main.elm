module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type Stone
    = Black
    | White


type alias CurrentTurn =
    Stone


type alias Board =
    List (List (Maybe Stone))


type alias Model =
    { board : Board
    , currentTurn : CurrentTurn
    }



-- VIEW


boardRow : List (Maybe Stone)
boardRow =
    [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]


init : Model
init =
    { board = [ boardRow, boardRow, boardRow, boardRow, boardRow, boardRow, boardRow, boardRow, boardRow, boardRow, boardRow, boardRow, boardRow, boardRow, boardRow, boardRow, boardRow, boardRow, boardRow ]
    , currentTurn = Black
    }


boardPositionView : ( Int, Int ) -> Maybe Stone -> Html Msg
boardPositionView coordinates maybeGp =
    case maybeGp of
        Nothing ->
            div [ class "board-pos empty-pos", onClick (Move coordinates) ] []

        Just gp ->
            case gp of
                Black ->
                    div [ class "board-pos occupied-pos black-pos" ] []

                White ->
                    div [ class "board-pos occupied-pos white-pos" ] []


boardRowView : Int -> List (Maybe Stone) -> Html Msg
boardRowView ypos row =
    div [ class "board-row" ] (List.indexedMap (\xpos stone -> boardPositionView ( xpos, ypos ) stone) row)


boardView : Board -> Html Msg
boardView board =
    div [ class "game-board" ] (List.indexedMap boardRowView board)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "zen go!" ]
        , h2 [] [ (model.currentTurn |> Debug.toString) ++ "'s turn to play" |> text ]
        , boardView model.board
        ]



-- UPDATE


type Msg
    = NoOp
    | Move ( Int, Int )



-- As this project develops, this function will contain all logic used to assess the validity of an attempted move
-- For now, only prevent a placed stone if there's already one there!


isStoneLegal : Stone -> Maybe Stone -> Bool
isStoneLegal stone currentPoint =
    case currentPoint of
        Nothing ->
            True

        Just _ ->
            False


playStone : Stone -> ( Int, Int ) -> Board -> Board
playStone stone ( xpos, ypos ) board =
    let
        placeStone currentPoint =
            if isStoneLegal stone currentPoint then
                Just stone

            else
                currentPoint

        newRow row =
            List.indexedMap
                (\pointIndex currPoint ->
                    if pointIndex == xpos then
                        placeStone currPoint

                    else
                        currPoint
                )
                row

        newBoard =
            List.indexedMap
                (\rowIndex currRow ->
                    if rowIndex == ypos then
                        newRow currRow

                    else
                        currRow
                )
                board
    in
    newBoard


update : Msg -> Model -> Model
update msg model =
    case msg of
        Move coords ->
            case model.currentTurn of
                Black ->
                    { model
                        | board = playStone Black coords model.board
                        , currentTurn = White
                    }

                White ->
                    { model
                        | board = playStone White coords model.board
                        , currentTurn = Black
                    }

        default ->
            model
