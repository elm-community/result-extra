module Result.Extra exposing (..)

{-| Convenience functions for working with `Result`.

# Common Helpers
@docs isOk, isErr, extract, unwrap, unpack, mapBoth, combine

# Alternatives
@docs or, orLazy, orElseLazy, orElse

-}


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


{-| Like the Boolean `||` this will return the first value that is
positive (`Ok`). However, unlike with `||`, both values will be
computed anyway (there is no short-circuiting).

    Ok 4      `or` Ok 5      == Ok 4
    Err "Oh!" `or` Ok 5      == Ok 5
    Ok 4      `or` Err "No!" == Ok 4
    Err "Oh!" `or` Err "No!" == Err "No!"

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
    case rb of
        Err _ ->
            fra ()

        Ok _ ->
            rb


{-| Strict version of `orElseLazy` (and at the same time,
piping-friendly version of `or`).

    orElse (Ok 4)      (Ok 5)      == Ok 5  -- crucial difference from `or`
    orElse (Err "Oh!") (Ok 5)      == Ok 5
    orElse (Ok 4)      (Err "No!") == Ok 4
    orElse (Err "Oh!") (Err "No!") == Err "Oh!"  -- also different from `or`

Also:

    String.toInt "Hello"
    |> orElse (String.toInt "42")
-}
orElse : Result e a -> Result e a -> Result e a
orElse ra rb =
    case rb of
        Err _ ->
            ra

        Ok _ ->
            rb
