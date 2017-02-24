module Main exposing (main)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Evt
import Keyboard.Combo as KeyCombo exposing (KeyCombo)
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
    , keys : KeyCombo.Model Action
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
    | KeyComboMsg KeyCombo.Msg


init : ( Model, Cmd Action )
init =
    { stack = []
    , head = 0
    , keys = KeyCombo.init KeyComboMsg keyMap
    }
        ! []


subscriptions : Model -> Sub Action
subscriptions model =
    KeyCombo.subscriptions model.keys


keyMap : List (KeyCombo Action)
keyMap =
    [ KeyCombo.combo1 KeyCombo.zero (Digit 0)
    , KeyCombo.combo1 KeyCombo.one (Digit 1)
    , KeyCombo.combo1 KeyCombo.two (Digit 2)
    , KeyCombo.combo1 KeyCombo.three (Digit 3)
    , KeyCombo.combo1 KeyCombo.four (Digit 4)
    , KeyCombo.combo1 KeyCombo.five (Digit 5)
    , KeyCombo.combo1 KeyCombo.six (Digit 6)
    , KeyCombo.combo1 KeyCombo.seven (Digit 7)
    , KeyCombo.combo1 KeyCombo.nine (Digit 9)
    , KeyCombo.combo2 ( KeyCombo.shift, KeyCombo.equals ) Add
    , KeyCombo.combo1 KeyCombo.minus Subtract
    , KeyCombo.combo2 ( KeyCombo.shift, KeyCombo.eight ) Multiply
    , KeyCombo.combo1 KeyCombo.semicolon Divide
    , KeyCombo.combo1 KeyCombo.enter Push
    , KeyCombo.combo1 KeyCombo.eight (Digit 8)
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

        KeyComboMsg msg ->
            { model | keys = KeyCombo.update msg model.keys } ! []


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
