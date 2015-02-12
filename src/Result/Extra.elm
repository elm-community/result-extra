module Result.Extra where
{-| Convenience functions for working with Result

# Common Helpers
@docs isOk, isErr

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
