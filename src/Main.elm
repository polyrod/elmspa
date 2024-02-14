module Main exposing (..)

import Array
import Browser exposing (..)
import Browser.Dom exposing (setViewport)
import Browser.Navigation exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import ElmLogo
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Lorem
import Task
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = Home
    | About
    | Impressum
    | Contact
    | Post Int
    | NotFound


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.Parser.map Home (s "elmspa")
        , Url.Parser.map About (s "elmspa" </> s "about")
        , Url.Parser.map Impressum (s "elmspa" </> s "impressum")
        , Url.Parser.map Contact (s "elmspa" </> s "contact")
        , Url.Parser.map Post (s "elmspa" </> s "article" </> int)
        ]


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse route url)


type alias Model =
    { key : Key
    , url : Url
    , route : Route
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | Page Route
    | NoOp


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


init : flags -> Url -> Key -> ( Model, Cmd msg )
init _ u k =
    ( { key = k, url = u, route = Home }, pushUrl k "/elmspa/" )


subs : Model -> Sub msg
subs _ =
    Sub.none


onUrlRequest : UrlRequest -> Msg
onUrlRequest r =
    LinkClicked r


onUrlChange : Url -> Msg
onUrlChange u =
    Page <| toRoute u


resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> NoOp) (setViewport 0 0)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        Page r ->
            ( { m | route = r }, resetViewport )

        LinkClicked r ->
            case r of
                Internal rt ->
                    ( m, pushUrl m.key (Url.toString rt) )

                External href ->
                    ( m, load href )

        NoOp ->
            ( m, Cmd.none )


view :
    Model
    -> Document Msg
view m =
    { title = "Hallo Elm", body = body m }


red : Color
red =
    rgb255 218 41 28


jet : Color
jet =
    rgb255 51 51 47


powderblue : Color
powderblue =
    rgb255 181 227 216


vanilla : Color
vanilla =
    rgb255 220 191 166


wheat : Color
wheat =
    rgb255 239 219 178


lsb : Color
lsb =
    rgb255 182 184 220


white : Color
white =
    rgb255 255 255 255


body : Model -> List (Html Msg)
body m =
    [ layout
        [ Background.color white
        , width fill
        , centerX
        , Font.family [ Font.typeface "Fira", Font.sansSerif ]
        , inFront (header m)
        ]
        (site m)
    ]


site : Model -> Element Msg
site m =
    column
        [ width fill
        , centerX
        , height fill
        ]
        (row
            [ Background.color red
            , width fill
            , alignTop
            ]
            [ header m ]
            :: content m
            ++ [ row
                    [ Background.color vanilla
                    , width fill
                    , alignBottom
                    , centerX
                    , height (fillPortion 1)
                    ]
                    [ footer m ]
               ]
        )


content : Model -> List (Element Msg)
content m =
    case m.route of
        Home ->
            [ row
                [ Background.color wheat
                , width fill
                , height (fillPortion 3)
                ]
                [ top "Prewit" m ]
            , row
                [ Background.color jet
                , width fill
                , alignTop
                , centerY
                , height (fillPortion 3)
                ]
                [ carousel ]
            , wrappedRow
                [ height (fillPortion 5)
                , alignTop
                , spacing 30
                , padding 60
                ]
                boxes
            ]

        About ->
            [ row
                [ Background.color wheat
                , width fill
                , height (fillPortion 3)
                ]
                [ top "About" m ]
            ]

        Impressum ->
            [ row
                [ Background.color wheat
                , width fill
                , height (fillPortion 3)
                ]
                [ top "Impressum" m ]
            ]

        Contact ->
            [ row
                [ Background.color wheat
                , width fill
                , height (fillPortion 3)
                ]
                [ top "Contact" m ]
            ]

        Post i ->
            let
                arr =
                    Array.fromList articles

                ma =
                    Array.get i arr
            in
            case ma of
                Just a ->
                    [ article a ]

                Nothing ->
                    []

        NotFound ->
            []


boxes : List (Element Msg)
boxes =
    List.indexedMap teasedArticle articles


header : a -> Element Msg
header m =
    row
        [ alignTop
        , width fill
        , padding 20
        , Background.color red
        ]
        [ column
            [ Font.size 32
            , Font.family [ Font.typeface "Oswald", Font.serif ]
            , Font.color jet
            , Font.extraBold
            , Font.glow jet 4.6
            , width fill
            ]
            [ text "MDPDev"
            , menu
            ]
        ]


menu : Element Msg
menu =
    row
        [ alignRight
        , alignBottom
        , spacing 20
        , Font.size 15
        , Font.color jet
        , Font.extraBold
        , Font.glow jet 2.6
        ]
        [ link []
            { url = "/elmspa/", label = text "Home" }
        , link []
            { url = "/elmspa/about", label = text "About" }
        , link []
            { url = "/elmspa/impressum", label = text "Impressum" }
        , link []
            { url = "/elmspa/contact", label = text "Contact" }
        ]


footer : a -> Element Msg
footer m =
    row
        [ alignBottom
        , width fill
        , padding 20
        ]
        [ column [ centerX, spacing 20 ]
            [ el [ centerX ] (ElmLogo.element 64)
            , el [ centerX, Font.size 14 ] <| text "Powered by Elm"
            ]
        ]


top : String -> a -> Element Msg
top h m =
    textColumn
        [ width fill
        , padding 60
        , height fill
        , spacing 10
        , centerX
        , centerY
        , Font.size 13
        ]
        [ paragraph
            [ Font.size 28
            , Font.family [ Font.typeface "Oswald" ]
            , Font.color jet
            , Font.extraBold
            , Font.glow jet 2.6
            ]
            [ text h ]
        , paragraph [] [ text <| Lorem.sentence 120 ]
        , paragraph [] [ text <| Lorem.sentence 80 ]
        , paragraph [] [ text <| Lorem.sentence 40 ]
        ]


type Article
    = Article { image : String, title : String, body : List String }


articles : List Article
articles =
    [ Article
        { image = "https://t3.ftcdn.net/jpg/02/95/44/22/360_F_295442295_OXsXOmLmqBUfZreTnGo9PREuAPSLQhff.jpg"
        , title = "Mischkate"
        , body = Lorem.paragraphs 60
        }
    , Article
        { image = "https://pics.craiyon.com/2023-07-23/ee9876ca464547a491a5b7f381f2d17b.webp"
        , title = "Dobopacina"
        , body = Lorem.paragraphs 40
        }
    , Article
        { image = "https://www.shutterstock.com/image-photo/funny-cat-flying-photo-playful-260nw-2315020963.jpg"
        , title = "Pistabol"
        , body = Lorem.paragraphs 50
        }
    , Article
        { image = "https://pics.craiyon.com/2023-07-23/ee9876ca464547a491a5b7f381f2d17b.webp"
        , title = "Dobopacina"
        , body = Lorem.paragraphs 70
        }
    , Article
        { image = "https://www.shutterstock.com/image-photo/funny-cat-flying-photo-playful-260nw-2315020963.jpg"
        , title = "Pistabol"
        , body = Lorem.paragraphs 50
        }
    ]


teasedArticle : Int -> Article -> Element Msg
teasedArticle i (Article a) =
    column
        [ Border.glow powderblue 1.2
        , Background.color jet
        , padding 20
        , spacing 10
        , Font.size 13
        , Font.color wheat
        , alignTop
        , width fill
        , height fill
        , centerX
        ]
        [ image [ width (fill |> minimum 120 |> maximum 300), centerX ] { src = a.image, description = "Horse" }
        , el
            [ alignLeft
            , Font.family [ Font.typeface "Oswald" ]
            , Font.size 18
            , Font.color wheat
            , Font.bold
            , Font.glow wheat 2.6
            ]
            (text a.title)
        , textColumn [ width shrink ] (List.take 2 <| List.map (\p -> paragraph [] [ text p ]) a.body)
        , link
            [ centerX
            , Font.size 14
            , Font.color powderblue
            , Font.glow powderblue 2.6
            ]
            { url = "article/" ++ String.fromInt i, label = text "read more" }
        ]


article : Article -> Element Msg
article (Article a) =
    textColumn
        [ Border.glow powderblue 1.2
        , Background.color jet
        , padding 60
        , spacing 10
        , Font.size 13
        , Font.color wheat
        , alignTop
        , width fill
        , height fill
        , centerX
        ]
        ([ image
            [ paddingEach { left = 0, right = 20, top = 0, bottom = 20 }
            , width (fill |> minimum 300 |> maximum 400)
            , alignLeft
            ]
            { src = a.image, description = "Horse" }
         , el
            [ padding 20
            , alignLeft
            , Font.family [ Font.typeface "Oswald" ]
            , Font.size 18
            , Font.color wheat
            , Font.bold
            , Font.glow wheat 2.6
            ]
            (text a.title)
         ]
            ++ List.map (\p -> paragraph [] [ text p ]) a.body
        )


carousel : Element Msg
carousel =
    el [ alignTop, centerX ] <|
        Element.html <|
            Html.div [ class "carousel-wrapper" ]
                [ div [ class "carousel-container" ]
                    [ div [ class "carousel" ]
                        [ div [ class "image-one" ] []
                        , div [ class "image-two" ] []
                        , div [ class "image-three" ] []
                        ]
                    ]
                ]
