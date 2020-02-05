module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (class, property, style)
import Html.Events exposing (on)
import Html.Keyed
import Json.Decode as Decode
import Json.Encode as Encode



---- MODEL ----


type alias Model =
    { height : Int
    , itemCount : Int
    , itemSize : Int
    , width : Int
    , scrollY : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { height = 150, itemCount = 1000, itemSize = 35, width = 300, scrollY = 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = ChangeScrollY Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeScrollY scrollY ->
            ( { model | scrollY = scrollY }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view =
    listView


listView : Model -> Html Msg
listView model =
    div
        [ style "position" "relative"
        , style "height" (px model.height)
        , style "width" (px model.width)
        , style "overflow" "auto"
        , style "will-change" "transform"
        , style "direction" "ltr"
        , class "List"
        , onScroll ChangeScrollY
        , scrollValue model.scrollY
        ]
        [ divK
            [ style "height" (model.itemCount * model.itemSize |> px)
            , style "width" "100%"
            ]
            (windowedListItems model)
        ]


windowedListItems : Model -> List ( String, Html Msg )
windowedListItems model =
    let
        lower =
            model.scrollY // model.itemSize

        upper =
            (model.height + model.scrollY) // model.itemSize

        lower_ =
            Basics.max 0 lower

        upper_ =
            Basics.min model.itemCount upper

        keyedListItemView n =
            ( String.fromInt n, listItemView model n )
    in
    List.map keyedListItemView <| List.range lower_ upper_


listItemView : Model -> Int -> Html Msg
listItemView model n =
    div
        [ style "height" (px model.itemSize)
        , style "width" "100%"
        , style "position" "absolute"
        , style "left" "0"
        , style "top" (n * model.itemSize |> px)
        , class (listItemClass n)
        ]
        [ text <| "Row " ++ String.fromInt n ]


divK : List (Attribute msg) -> List ( String, Html msg ) -> Html msg
divK =
    Html.Keyed.node "div"


listItemClass : Int -> String
listItemClass n =
    if modBy 2 n == 0 then
        "ListItemEven"

    else
        "ListItemOdd"


px : Int -> String
px n =
    String.fromInt n ++ "px"


onScroll : (Int -> msg) -> Attribute msg
onScroll fn =
    on "scroll" (Decode.map fn (Decode.at [ "target", "scrollTop" ] Decode.int))


scrollValue : Int -> Attribute msg
scrollValue scrollTop =
    property "scrollTop" (Encode.int scrollTop)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
