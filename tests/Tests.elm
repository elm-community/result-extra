module Tests exposing (all)

import Expect
import Result.Extra exposing (andMap, isOk, partition, singleton, toTask)
import Task
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Result.Extra"
        [ commonHelperTests
        , applyingTests
        , toTaskTests
        ]


commonHelperTests : Test
commonHelperTests =
    describe "Common Helpers"
        [ test "isOk - Ok" <|
            \_ ->
                Expect.true "Expected Ok value to return True" (isOk <| Ok 2)
        , test "isOk - Err" <|
            \_ -> Expect.false "Expected Err value to return False" (isOk <| Err 42)
        , partitionTests
        ]


partitionTests : Test
partitionTests =
    describe "partition"
        [ test "empty list" <|
            \_ ->
                Expect.equal (partition []) ( [], [] )
        , test "only Ok" <|
            \_ ->
                Expect.equal (partition [ Ok 99 ]) ( [ 99 ], [] )
        , test "only Err" <|
            \_ ->
                Expect.equal (partition [ Err 99 ]) ( [], [ 99 ] )
        , test "mixed list" <|
            \_ ->
                Expect.equal (partition [ Ok 99, Err "Nope", Ok -5 ]) ( [ 99, -5 ], [ "Nope" ] )
        ]


applyingTests : Test
applyingTests =
    describe "Applying"
        [ test "singleton" <|
            \_ ->
                Expect.equal (singleton 42) (Ok 42)
        , andMapTests
        ]


andMapTests : Test
andMapTests =
    describe "andMap"
        [ test "Err -> Err -> Err" <|
            \_ ->
                Expect.equal (Err "Oh" |> andMap (Err "No!")) (Err "Oh")
        , test "Err -> Ok -> Err" <|
            \_ ->
                Expect.equal (Err "Oh" |> andMap (Ok 2)) (Err "Oh")
        , test "Ok -> Err -> Err" <|
            \_ ->
                Expect.equal (Ok ((+) 1) |> andMap (Err "No!")) (Err "No!")
        , test "Ok -> Ok -> Ok" <|
            \_ ->
                Expect.equal (Ok ((+) 1) |> andMap (Ok 2)) (Ok 3)
        ]


toTaskTests : Test
toTaskTests =
    describe "toTask"
        [ test "Ok" <|
            \_ ->
                Expect.equal (toTask (Ok 4)) (Task.succeed 4)
        , test "Err" <|
            \_ ->
                Expect.equal (toTask (Err "Oh")) (Task.fail "Oh")
        ]
