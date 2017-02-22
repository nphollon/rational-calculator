module Rational exposing (..)


type Rational
    = Q Int Int


zero : Rational
zero =
    Q 0 1


one : Rational
one =
    Q 1 1


indeterminate : Rational
indeterminate =
    Q 0 0


infinity : Rational
infinity =
    Q 1 0


negativeInfinity : Rational
negativeInfinity =
    Q -1 0


int : Int -> Rational
int i =
    Q i 1


toFloat : Rational -> Float
toFloat (Q n d) =
    Basics.toFloat n / Basics.toFloat d


numerator : Rational -> Int
numerator (Q n _) =
    n


denominator : Rational -> Int
denominator (Q _ d) =
    d


isInt : Rational -> Bool
isInt (Q _ d) =
    d == 1


multiply : Rational -> Rational -> Rational
multiply (Q n1 d1) (Q n2 d2) =
    rat (n1 * n2) (d1 * d2)


divide : Rational -> Rational -> Rational
divide (Q n1 d1) (Q n2 d2) =
    rat (n1 * d2) (d1 * n2)


pow : Int -> Rational -> Rational
pow exponent base =
    case compare exponent 0 of
        EQ ->
            if base == zero then
                indeterminate
            else
                one

        GT ->
            List.repeat (exponent - 1) base
                |> List.foldl multiply base

        LT ->
            pow -exponent (divide one base)


add : Rational -> Rational -> Rational
add (Q n1 d1) (Q n2 d2) =
    rat (n1 * d2 + n2 * d1) (d1 * d2)


subtract : Rational -> Rational -> Rational
subtract (Q n1 d1) (Q n2 d2) =
    rat (n1 * d2 - n2 * d1) (d1 * d2)


rat : Int -> Int -> Rational
rat n d =
    case ( compare n 0, compare d 0 ) of
        ( EQ, EQ ) ->
            indeterminate

        ( GT, EQ ) ->
            infinity

        ( LT, EQ ) ->
            negativeInfinity

        ( EQ, _ ) ->
            zero

        ( _, GT ) ->
            let
                gcd =
                    greatestCommonDivisor (abs n) d
            in
                Q (n // gcd) (d // gcd)

        ( _, LT ) ->
            let
                gcd =
                    greatestCommonDivisor (abs n) -d
            in
                Q (-n // gcd) (-d // gcd)


greatestCommonDivisor : Int -> Int -> Int
greatestCommonDivisor a b =
    let
        euclidStep x y =
            if x % y == 0 then
                y
            else
                euclidStep y (x % y)
    in
        euclidStep (max a b) (min a b)
