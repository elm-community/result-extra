module Result.Extra where
{-| Convenience functions for working with Result

# Common Helpers
@docs isOk, isErr, flatten

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
flatten : (e -> a) -> Result e a -> a
flatten f x = case x of
                Ok a -> a
                Err e -> f e
