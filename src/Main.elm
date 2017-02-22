module Main exposing (main)

import Char exposing (KeyCode)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Evt
import Keyboard
import Rational as Q exposing (Rational)


main : Program Never Model Action
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { stack : List Rational
    , head : Int
    }


type Action
    = Digit Int
    | Divide
    | Multiply
    | Square
    | Reciprocate
    | Negate
    | Subtract
    | Add
    | Push
    | Pop
    | Duplicate
    | RollUp
    | RollDown
    | Swap
    | DoNothing


init : ( Model, Cmd Action )
init =
    { stack = []
    , head = 0
    }
        ! []


subscriptions : Model -> Sub Action
subscriptions model =
    Keyboard.downs <|
        \keyCode ->
            Dict.get keyCode keyMap
                |> Maybe.withDefault DoNothing


keyMap : Dict KeyCode Action
keyMap =
    Dict.fromList
        [ ( Char.toCode '0', Digit 0 )
        , ( Char.toCode '1', Digit 1 )
        , ( Char.toCode '2', Digit 2 )
        , ( Char.toCode '3', Digit 3 )
        , ( Char.toCode '4', Digit 4 )
        , ( Char.toCode '5', Digit 5 )
        , ( Char.toCode '6', Digit 6 )
        , ( Char.toCode '7', Digit 7 )
        , ( Char.toCode '8', Digit 8 )
        , ( Char.toCode '9', Digit 9 )
        , ( 13, Push )
        ]


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Digit i ->
            addDigit i model ! []

        Push ->
            push model ! []

        Pop ->
            pop model ! []

        Duplicate ->
            duplicate model ! []

        Swap ->
            swap model ! []

        RollUp ->
            rollUp model ! []

        RollDown ->
            rollDown model ! []

        Add ->
            binary Q.add model ! []

        Subtract ->
            binary Q.subtract model ! []

        Negate ->
            unary (Q.subtract Q.zero) model ! []

        Multiply ->
            binary Q.multiply model ! []

        Divide ->
            binary Q.divide model ! []

        Square ->
            unary (Q.pow 2) model ! []

        Reciprocate ->
            unary (Q.divide Q.one) model ! []

        DoNothing ->
            model ! []


addDigit : Int -> Model -> Model
addDigit digit model =
    { model | head = digit + 10 * model.head }


push : Model -> Model
push model =
    { model
        | head = 0
        , stack = Q.int model.head :: model.stack
    }


pop : Model -> Model
pop model =
    { model | stack = List.drop 1 model.stack }


duplicate : Model -> Model
duplicate model =
    { model
        | stack =
            case model.stack of
                head :: tail ->
                    head :: head :: tail

                [] ->
                    []
    }


swap : Model -> Model
swap model =
    { model
        | stack =
            case model.stack of
                x :: y :: tail ->
                    y :: x :: tail

                x :: [] ->
                    [ Q.zero, x ]

                [] ->
                    []
    }


rollUp : Model -> Model
rollUp model =
    case unconsEnd model.stack of
        Nothing ->
            model

        Just ( last, front ) ->
            { model | stack = last :: front }


unconsEnd : List a -> Maybe ( a, List a )
unconsEnd =
    List.foldr
        (\i acc ->
            case acc of
                Just ( last, front ) ->
                    Just ( last, i :: front )

                Nothing ->
                    Just ( i, [] )
        )
        Nothing


rollDown : Model -> Model
rollDown model =
    case model.stack of
        [] ->
            model

        head :: [] ->
            model

        head :: tail ->
            { model | stack = tail ++ [ head ] }


binary : (Rational -> Rational -> Rational) -> Model -> Model
binary operation model =
    { model
        | stack =
            case model.stack of
                second :: first :: rest ->
                    operation first second :: rest

                second :: [] ->
                    [ operation Q.zero second ]

                [] ->
                    [ operation Q.zero Q.zero ]
    }


unary : (Rational -> Rational) -> Model -> Model
unary operation model =
    { model
        | stack =
            case model.stack of
                head :: tail ->
                    operation head :: tail

                [] ->
                    [ operation Q.zero ]
    }


view : Model -> Html Action
view model =
    Html.div
        [ Attr.style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "flex-start" )
            ]
        ]
        [ stackView model.stack
        , inputField model.head
        , inputControls
        ]


stackView : List Rational -> Html a
stackView stack =
    let
        toCell number =
            Html.div
                [ Attr.style
                    [ ( "display", "flex" )
                    , ( "justify-content", "space-between" )
                    ]
                ]
                [ textBox (cellText number)
                , textBox (approx number)
                ]

        cells =
            List.map toCell stack
    in
        Html.div
            [ Attr.style
                [ ( "display", "flex" )
                , ( "flex-direction", "column-reverse" )
                ]
            ]
            cells


cellText : Rational -> String
cellText number =
    if Q.isInt number then
        toString (Q.numerator number)
    else
        [ toString (Q.numerator number)
        , " / "
        , toString (Q.denominator number)
        ]
            |> String.concat


approx : Rational -> String
approx =
    Q.toFloat >> toString


inputField : Int -> Html a
inputField input =
    Html.div []
        [ textBox (toString input) ]


textBox : String -> Html a
textBox text =
    Html.div
        [ Attr.style [ ( "margin", "5px 25px" ) ] ]
        [ Html.text text ]


inputControls : Html Action
inputControls =
    let
        mathButtons =
            Html.div []
                [ button Add "+"
                , button Subtract "-"
                , button Multiply "x"
                , button Divide "/"
                , button Square "^2"
                , button Negate "±"
                , button Reciprocate "1/x"
                ]

        stackButtons =
            Html.div []
                [ button Push "Push"
                , button Duplicate "Dup"
                , button Pop "Pop"
                , button Swap "Swap"
                , button RollDown "Roll Dn"
                , button RollUp "Roll Up"
                ]
    in
        Html.div []
            [ stackButtons
            , mathButtons
            ]


button : a -> String -> Html a
button action label =
    Html.button [ Evt.onClick action ] [ Html.text label ]
