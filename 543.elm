module Main exposing (..)

import Html exposing (..)
import Memo exposing (memo)
import List.Extra exposing (lift2, unique)


main : Html a
main =
    let
        n =
            100

        primes =
            let
                primes' n =
                    let
                        sieve xs =
                            case xs of
                                p :: ps ->
                                    p :: sieve (List.filter (\x -> x % p > 0) ps)

                                _ ->
                                    xs
                    in
                        sieve [2..n]
            in
                memoize primes' [ n ]

        sums =
            let
                allSums i =
                    case i of
                        1 ->
                            primes n

                        _ ->
                            lift2 (\x a -> x + a) (primes n) (sums (i - 1))
                                |> unique
                                |> List.filter ((>=) n)
                                |> List.sort
                                |> Debug.log "test"
            in
                memoize allSums [1..n]

        p : Int -> Int -> Int
        p n k =
            if List.member n (sums k) then
                1
            else
                0

        s : Int -> Int
        s n =
            case n of
                0 ->
                    0

                _ ->
                    List.sum (List.map (p n) [1..(n // 2)]) + s (n - 1)
    in
        text <| toString <| s n


fib : Int -> Int
fib n =
    let
        fib' n =
            if n < 2 then
                1
            else
                mfib (n - 1) + mfib (n - 2)

        mfib =
            memoize fib' [2..n - 2]
    in
        fib' n


memoize : (comparable -> b) -> List comparable -> comparable -> b
memoize fun args =
    let
        memoized =
            memo fun args
    in
        \arg ->
            case memoized arg of
                Just val ->
                    val

                Nothing ->
                    fun arg
