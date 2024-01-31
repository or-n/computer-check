module Main (main) where

import Term
import Decode
import Constraints

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
            print (infer term)
        _ -> do
            putStr "Counldn't decode: "
            putStrLn text