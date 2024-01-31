module Main (main) where

import Term
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
            putStrLn ""
        _ -> do
            putStr "Counldn't decode: "
            putStrLn text