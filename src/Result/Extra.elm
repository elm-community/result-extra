module Result.Extra where
{-| Convenience functions for working with Result
-}

isOk : Result a e -> Bool
isOk x = case x of
          Ok  _ -> True
          Err _ -> False

isErr : Result a e -> Bool
isErr x = case x of
            Ok  _ -> False
            Err _ -> True
