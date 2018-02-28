module Tests exposing (..)

import Expect
import Test exposing (..)


-- Vending Machine:  https://code.joejag.com/coding-dojo/vending-machine/
--
-- NICKEL(0.05), DIME(0.10), QUARTER(0.25), DOLLAR(1.00) - insert money
-- COIN RETURN - returns all inserted money
-- GET-A, GET-B, GET-C - select item A ($0.65), B ($1), or C ($1.50)
-- SERVICE - a service person opens the machine and sets the available changeand items


type Money
    = Nickel
    | Dime
    | Quarter
    | Dollar


moneyValue : Money -> Float
moneyValue money =
    case money of
        Nickel ->
            0.05

        Dime ->
            0.1

        Quarter ->
            0.25

        Dollar ->
            1



-- sumMoney = List.map moneyValue >> List.sum


sumMoney : List Money -> Float
sumMoney s =
    case s of
        m :: rest ->
            moneyValue m + sumMoney rest

        [] ->
            0


coinReturn : List Money -> List Money
coinReturn money =
    case money of
        Dollar :: rest ->
            [ Quarter, Quarter, Quarter, Quarter ] ++ coinReturn rest

        head :: tail ->
            head :: coinReturn tail

        [] ->
            money


all : Test
all =
    describe "Vending Machine"
        [ describe "coinReturn"
            [ test "Return a dime" <|
                \_ ->
                    Expect.equal [ Dime ] (coinReturn [ Dime ])
            , test "Return a Dollar" <|
                \_ ->
                    Expect.equal
                        [ Quarter, Quarter, Quarter, Quarter ]
                        (coinReturn [ Dollar ])
            , test "Return a Dollar and a Dime" <|
                \_ ->
                    coinReturn [ Dollar, Dime ]
                        |> Expect.equal
                            [ Quarter, Quarter, Quarter, Quarter, Dime ]
            , test "Return a Dime and a Dollar" <|
                \_ ->
                    coinReturn [ Dime, Dollar ]
                        |> Expect.equal [ Dime, Quarter, Quarter, Quarter, Quarter ]
            ]
        , describe "moneyValue"
            [ test "Nickel" <|
                \_ ->
                    Expect.equal 0.05 (moneyValue Nickel)
            , test "Dime" <|
                \_ ->
                    Expect.equal 0.1 (moneyValue Dime)
            , test "Quarter" <|
                \_ ->
                    Expect.equal 0.25 (moneyValue Quarter)
            , test "Dollar" <|
                \_ ->
                    Expect.equal 1.0 (moneyValue Dollar)
            ]
        , describe "sumMoney"
            [ test "Sum empty pocket" <|
                \_ ->
                    Expect.equal 0 (sumMoney [])
            , test "Sum single dollar" <|
                \_ ->
                    Expect.equal 1 (sumMoney [ Dollar ])
            , test "Sum single quarter" <|
                \_ ->
                    Expect.equal 0.25 (sumMoney [ Quarter ])
            , test "Sum a few values" <|
                \_ ->
                    Expect.equal 1.25 (sumMoney [ Quarter, Dollar ])
            , test "Sum of three values" <|
                \_ ->
                    Expect.equal 1.35 (sumMoney [ Dime, Quarter, Dollar ])
            ]
        ]
