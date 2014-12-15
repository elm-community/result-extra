module Result.Extra where
{-| Convenience functions for working with Result
-}

isOk : Result -> Bool
isOk x = case x of
          Ok  _ -> True
          Err _ -> False

isErr : Result -> Bool
isErr x = case x of
            Ok  _ -> False
            Err _ -> True
