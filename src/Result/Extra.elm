module Result.Extra
    exposing
        ( andMap
        , combine
        , combineBoth
        , combineFirst
        , combineMap
        , combineMapBoth
        , combineMapFirst
        , combineMapSecond
        , combineSecond
        , extract
        , isErr
        , isOk
        , join
        , mapBoth
        , merge
        , or
        , orElse
        , orElseLazy
        , orLazy
        , singleton
        , unpack
        , unwrap
        )

{-| Convenience functions for working with `Result`.


# Packing and unpacking

@docs isOk, isErr, extract, unwrap, unpack, mapBoth, merge, join


# Combining multiple results


## In lists

@docs combine, combineMap


## In tuples

@docs combineFirst, combineSecond, combineBoth, combineMapFirst, combineMapSecond, combineMapBoth


# Applying

@docs singleton, andMap


# Alternatives

@docs or, orLazy, orElseLazy, orElse

-}

import Basics.Extra exposing (flip)



-- UNPACKING --


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



-- TRAVERSABLE --


{-| Combine a list of results into a single result (holding a list).
Also known as `sequence` on lists.
-}
combine : List (Result x a) -> Result x (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])


{-| Map a function producing results on a list
and combine those into a single result (holding a list).
Also known as `traverse` on lists.

    combineMap f xs == combine (List.map f xs)

-}
combineMap : (a -> Result x b) -> List a -> Result x (List b)
combineMap f =
    combine << List.map f


{-| Pull a result out of the _first_ element of a tuple
and combine it into a result holding the tuple's values.
-}
combineFirst : ( Result x a, c ) -> Result x ( a, c )
combineFirst ( rx, y ) =
    Result.map (flip Tuple.pair y) rx


{-| Pull a result out of the _second_ element of a tuple
and combine it into a result holding the tuple's values.
Also known as `sequence` on tuples.
-}
combineSecond : ( c, Result x a ) -> Result x ( c, a )
combineSecond ( x, ry ) =
    Result.map (Tuple.pair x) ry


{-| Combine all results in a tuple
into a single result holding the tuple's values.
Also know as `bisequence` on tuples.
-}
combineBoth : ( Result x a, Result x b ) -> Result x ( a, b )
combineBoth ( rx, ry ) =
    Result.map2 Tuple.pair rx ry


{-| Map a function producing results on the _first_ element of a tuple
and then pull it out using `combineFirst`.
Also know as `sequence` on tuples.

    combineMapFirst f ( x, y )
        == combineFirst (Tuple.mapFirst f ( x, y ))
        == Result.map (flip Tuple.pair y) (f x)

-}
combineMapFirst : (a -> Result x b) -> ( a, c ) -> Result x ( b, c )
combineMapFirst f =
    combineFirst << Tuple.mapFirst f


{-| Map a function producing results on the _second_ element of a tuple
and then pull it out using `combineSecond`.
Also know as `traverse` on tuples.

    combineMapSecond f ( x, y )
        == combineSecond (Tuple.mapSecond f ( x, y ))
        == Result.map (Tuple.pair x) (f y)

-}
combineMapSecond : (a -> Result x b) -> ( c, a ) -> Result x ( c, b )
combineMapSecond f =
    combineSecond << Tuple.mapSecond f


{-| Map a function producing results on the _both_ elements of a tuple
and then pull them out using `combineBoth`.
Also know as `bitraverse` on tuples.

    combineMapBoth f g ( x, y )
        == combineBoth (Tuple.mapBoth f g ( x, y ))
        == Result.map2 Tuple.pair (f x) (g y)

-}
combineMapBoth : (a -> Result x c) -> (b -> Result x d) -> ( a, b ) -> Result x ( c, d )
combineMapBoth f g =
    combineBoth << Tuple.mapBoth f g



-- APPLICATIVE --


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

    Err "Oh" |> andMap (Err "No!")   == Err "Oh"
    Err "Oh" |> andMap (Ok 2)        == Err "Oh"
    Ok ((+) 1) |> andMap (Err "No!") == Err "No!"
    Ok ((+) 1) |> andMap (Ok 2)      == Ok 3

-}
andMap : Result e a -> Result e (a -> b) -> Result e b
andMap ra rb =
    case ( ra, rb ) of
        ( _, Err x ) ->
            Err x

        ( o, Ok fn ) ->
            Result.map fn o



-- ALTERNATIVE --


{-| Like the Boolean `||` this will return the first value that is
positive (`Ok`). However, unlike with `||`, both values will be
computed anyway (there is no short-circuiting).

    or (Ok 4)      (Ok 5)      == Ok 4
    or (Err "Oh!") (Ok 5)      == Ok 5
    or (Ok 4)      (Err "No!") == Ok 4
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



-- OTHER --


{-| Eliminate Result when error and success have been mapped to the same
type, such as a message type.

    merge (Ok 4)   == 4
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


{-| Join contained results with the same error into one result.

Usefull if you have a "result in a result":

    join <| Ok (Ok 4) == Ok 4
    join <| Ok (Err "message") == Err "message"

-}
join : Result x (Result x a) -> Result x a
join r =
    case r of
        Err x ->
            Err x

        Ok (Err x) ->
            Err x

        Ok (Ok a) ->
            Ok a
