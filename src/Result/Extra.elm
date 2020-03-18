module Result.Extra exposing
    ( isOk, isErr, extract, unwrap, unpack, error, mapBoth, combine, merge, partition, filter
    , singleton, andMap
    , or, orLazy, orElseLazy, orElse
    , toTask
    )

{-| Convenience functions for working with `Result`.


# Common Helpers

@docs isOk, isErr, extract, unwrap, unpack, error, mapBoth, combine, merge, partition, filter


# Applying

@docs singleton, andMap


# Alternatives

@docs or, orLazy, orElseLazy, orElse


# Conversions

@docs toTask

-}

import Task exposing (Task)


{-| Check whether the result is `Ok` without unwrapping it.
-}
isOk : Result e a -> Bool
isOk x =
    case x of
        Ok _ ->
            True

        Err _ ->
            False


{-| Check whether the result is `Err` without unwrapping it.
-}
isErr : Result e a -> Bool
isErr x =
    case x of
        Ok _ ->
            False

        Err _ ->
            True


{-| Turn a `Result e a` to an `a`, by applying the conversion
function specified to the `e`.
-}
extract : (e -> a) -> Result e a -> a
extract f x =
    case x of
        Ok a ->
            a

        Err e ->
            f e


{-| Convert a `Result e a` to a `b` by applying a function if
the `Result` is `Ok` or using the provided default value if it
is an `Err`.
-}
unwrap : b -> (a -> b) -> Result e a -> b
unwrap defaultValue okFunc result =
    case result of
        Ok ok ->
            okFunc ok

        Err _ ->
            defaultValue


{-| Convert a `Result e a` to a `b` by applying either the first
function if the `Result` is an `Err` or the second function if the
`Result` is `Ok`. Both of these functions must return the same type.
-}
unpack : (e -> b) -> (a -> b) -> Result e a -> b
unpack errFunc okFunc result =
    case result of
        Ok ok ->
            okFunc ok

        Err err ->
            errFunc err


{-| Convert to a Maybe containing the error, if there is one.

    parseInt : String -> Result ParseError Int

    maybeParseError : String -> Maybe ParseError
    maybeParseError string =
        error (parseInt string)

-}
error : Result e a -> Maybe e
error result =
    case result of
        Ok _ ->
            Nothing

        Err err ->
            Just err


{-| Apply the first argument function to an `Err` and the second
argument function to an `Ok` of a `Result`.
-}
mapBoth : (e -> f) -> (a -> b) -> Result e a -> Result f b
mapBoth errFunc okFunc result =
    case result of
        Ok ok ->
            Ok <| okFunc ok

        Err err ->
            Err <| errFunc err


{-| Combine a list of results into a single result (holding a list).
-}
combine : List (Result x a) -> Result x (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])


{-| Create a `singleton` from a value to an `Result` with a `Ok`
of the same type. Also known as `pure`. You can use the `Err`
constructor for a singleton of the `Err` variety.

    singleton 2 == Ok 2

-}
singleton : a -> Result e a
singleton =
    Ok


{-| Apply the function that is inside `Result` to a value that is inside
`Result`. Return the result inside `Result`. If one of the `Result`
arguments is `Err e`, return `Err e`. Also known as `apply`.

    Err "Oh" |> andMap (Err "No!") == Err "Oh"

    Err "Oh" |> andMap (Ok 2) == Err "Oh"

    Ok ((+) 1) |> andMap (Err "No!") == Err "No!"

    Ok ((+) 1) |> andMap (Ok 2) == Ok 3

-}
andMap : Result e a -> Result e (a -> b) -> Result e b
andMap ra rb =
    case ( ra, rb ) of
        ( _, Err x ) ->
            Err x

        ( o, Ok fn ) ->
            Result.map fn o


{-| Like the Boolean `||` this will return the first value that is
positive (`Ok`). However, unlike with `||`, both values will be
computed anyway (there is no short-circuiting).

    or (Ok 4) (Ok 5) == Ok 4

    or (Err "Oh!") (Ok 5) == Ok 5

    or (Ok 4) (Err "No!") == Ok 4

    or (Err "Oh!") (Err "No!") == Err "No!"

As the last example line shows, the second error is returned if both
results are erroneous.

-}
or : Result e a -> Result e a -> Result e a
or ra rb =
    case ra of
        Err _ ->
            rb

        Ok _ ->
            ra


{-| Non-strict version of `or`. The second argument will only be
evaluated if the first argument is an `Err`.
-}
orLazy : Result e a -> (() -> Result e a) -> Result e a
orLazy ra frb =
    case ra of
        Err _ ->
            frb ()

        Ok _ ->
            ra


{-| Piping-friendly version of `orLazy`. The first argument will only
be evaluated if the second argument is an `Err`. Example use:

    String.toInt "Hello"
        |> orElseLazy (\() -> String.toInt "42")

-}
orElseLazy : (() -> Result e a) -> Result e a -> Result e a
orElseLazy fra rb =
    orLazy rb fra


{-| Strict version of `orElseLazy` (and at the same time,
piping-friendly version of `or`).

    orElse (Ok 4) (Ok 5) == Ok 5 -- crucial difference from `or`

    orElse (Err "Oh!") (Ok 5) == Ok 5

    orElse (Ok 4) (Err "No!") == Ok 4

    orElse (Err "Oh!") (Err "No!") == Err "Oh!" -- also different from `or`

Also:

    String.toInt "Hello"
        |> orElse (String.toInt "42")

-}
orElse : Result e a -> Result e a -> Result e a
orElse ra rb =
    or rb ra


{-| Eliminate Result when error and success have been mapped to the same
type, such as a message type.

    merge (Ok 4) == 4

    merge (Err -1) == -1

More pragmatically:

    type Msg
        = UserTypedInt Int
        | UserInputError String

    msgFromInput : String -> Msg
    msgFromInput =
        String.toInt
            >> Result.mapError UserInputError
            >> Result.map UserTypedInt
            >> Result.Extra.merge

-}
merge : Result a a -> a
merge r =
    case r of
        Ok rr ->
            rr

        Err rr ->
            rr


{-| Partition a list of Results into two lists of values (successes
and failures), much as List.partition takes a predicate and splits
a list based on whether the predicate indicates success or failure.

    partition ( Ok 4, Err "no", Err "hi" ) == ( [ 4 ], [ "no", "hi" ] )

    partition ( Err 7.1, Ok 'k', Err 9.0, Ok 'p' ) == ( [ 'k', 'p' ], [ 7.1, 9.0 ] )

-}
partition : List (Result e a) -> ( List a, List e )
partition rs =
    List.foldr
        (\r ( succ, err ) ->
            case r of
                Ok v ->
                    ( v :: succ, err )

                Err v ->
                    ( succ, v :: err )
        )
        ( [], [] )
        rs


{-| Take a `Result` and a predicate function and return a `Result` with the original value when a predicate matches.

    filter "is not 1" (\v -> v == 1) (Ok 1) == Ok 1

    filter "is not 2" (\v -> v == 2) (Ok 1) == Err "is not 2"

-}
filter : e -> (a -> Bool) -> Result e a -> Result e a
filter err predicate result =
    case Result.map predicate result of
        Ok True ->
            result

        Ok False ->
            Err err

        Err _ ->
            result


{-| Convert a `Result` to a `Task` that will fail or succeed immediately.

    toTask (Ok 4) == Task.succeed 4

    toTask (Err "msg") == Task.fail "msg"

This can be helpful when the value of a succeeding Task needs to be decoded, but
a failure to decode should result in a failing `Task`, not a succeeding Task
containing a `Result.Err`:

andThenDecode : (a -> Result x b) -> Task x a -> Task x b
andThenDecode decode =
Task.andThen (decode >> Result.Extra.toTask)

-}
toTask : Result x a -> Task x a
toTask result =
    case result of
        Ok a ->
            Task.succeed a

        Err x ->
            Task.fail x
