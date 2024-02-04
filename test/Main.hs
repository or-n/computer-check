module Main (main) where

import Term
import TermType
import Decode
import Constraints
import System.Timeout (timeout)
import Run

main :: IO ()
main = do
    test "a: b: a"
    test "a: a: a"
    test "x: x"
    test "(x: x) (x: x)"
    test "x: x x"
    --test "@f := (x: f x) in f 69"
    test "1 + 1"
    test "0 z? 1 : 2"
    test "2 - 1"
    test "(x: x z? 42 : (f (x - 1))) 2"
    test "@f := (x: x z? 42 : (f (x - 1))) in f 2"
    test "(2137, (69, ())).rest"
    test "((2137, (69, ())).rest).top"
    test "x: (x, ())"
    test "x: x.top"
    test "x: xs: (x, xs)"
    test "()"
    test "() e? 1 : 2"
    test "@x := (&2137) in @_ := (x <- ((*x) + 1)) in *x"
    test "@l := (&()) in @_ := (l <- ((x: x), ())) in (*l)"
    test "@l := (&()) in @_ := (l <- ((x: x), ())) in ((*l).top) + 1"
    --test_equations "@x := (&2137) in *x"

{-
test_equations code = do
    r <- gen_equations empty_env (Generic "target") <$> decode code
    case r of
        Just equations -> do
            x <- resolve equations
            print x-}

test :: String -> IO ()
test text = do
    case decode text of
        Right term -> do
            putStr "Decoded: "
            print term
            putStr "Renamed: "
            print (unique_names term)
            putStr "Inferred type: "
            t <- timeout 1000000 (infer term)
            print t
            putStrLn "Reduction steps: "
            _ <- timeout 1000000 (print_strict_steps term)
            return ()
        _ -> do
            putStr "Couldn't decode: "
            putStrLn text
    putStrLn ""