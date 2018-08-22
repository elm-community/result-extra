module Tests exposing (..)

import Test exposing (..)
import Expect
import Result.Extra exposing (..)


all : Test
all =
    describe "Result.Extra Suite"
        [ describe "Basic Result.Extra tests"
            [ test "singleton" <|
                \_ ->
                    Expect.equal (singleton 42) (Ok 42)
            , test "andMap Err Err" <|
                \_ ->
                    Expect.equal (Err "Oh" |> andMap (Err "No!")) (Err "Oh")
            , test "andMap Err O" <|
                \_ ->
                    Expect.equal (Err "Oh" |> andMap (Ok 2)) (Err "Oh")
            , test "andMap Ok Err" <|
                \_ ->
                    Expect.equal (Ok ((+) 1) |> andMap (Err "No!")) (Err "No!")
            , test "andMap Ok Ok" <|
                \_ ->
                    Expect.equal (Ok ((+) 1) |> andMap (Ok 2)) (Ok 3)
            ]
        ]
