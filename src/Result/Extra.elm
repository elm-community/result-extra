module Result.Extra where
{-| Convenience functions for working with Result

# Common Helpers
@docs isOk, isErr, extract, mapBoth, withDefault, combine

-}

{-| Check whether the result is `Ok` without unwrapping it.
-}
isOk : Result e a -> Bool
isOk x = case x of
          Ok  _ -> True
          Err _ -> False

{-| Check whether the result is `Err` without unwrapping it.
-}
isErr : Result e a -> Bool
isErr x = case x of
            Ok  _ -> False
            Err _ -> True

{-| Turn a `Result e a` to an `a`, by applying the conversion
function specified to the `e`.
-}
extract : (e -> a) -> Result e a -> a
extract f x = case x of
                Ok a -> a
                Err e -> f e

{-| Convert a `Result e a` to a `b` by applying either a function
if the `Result` is an `Err` or a function if the `Result` is `Ok`.
Both of these functions must return the same type.
-}
mapBoth : (e -> b) -> (a -> b) -> Result e a -> b
mapBoth errFunc okFunc result =
    case result of
        Ok ok ->
          okFunc ok

        Err err ->
          errFunc err

{-| Returns a `Result`'s contents if the `Result` is an `Ok`,
or the given default value if the `Result` is an `Err`.
This is basically the same as running `Result.toMaybe` and then `Maybe.withDefault`.

    0 == Result.withDefault 0 (String.toInt "this is not a valid integer!")
-}
withDefault : a -> Result x a -> a
withDefault default result =
    case result of
        Ok value ->
            value

        Err _ ->
            default

{-| Combine a list of results into a single result (holding a list).
-}
combine : List (Result x a) -> Result x (List a)
combine = List.foldr (Result.map2 (::)) (Ok [])
