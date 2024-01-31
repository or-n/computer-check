{-# LANGUAGE LambdaCase #-}
module Run where

import Term
import System.Timeout (timeout)

substitute :: String -> Term -> Term -> Term
substitute substituted_name term = go where
	go = \case
		Refer name | name == substituted_name ->
			term
		Supply process input ->
			Supply (go process) (go input)
		Assume name usage | name /= substituted_name ->
			Assume name (go usage)
		x -> x

run_strict :: Term -> Term
run_strict term = maybe term run_strict (strict_step term)

strict_step :: Term -> Maybe Term
strict_step term = do
	(process, input) <- get_supply term
	(name, usage) <- get_assume (run_strict process)
	Just $ substitute name input usage

print_strict_steps :: Term -> IO ()
print_strict_steps term = case strict_step term of
	Just new_term -> do
		print new_term
		print_strict_steps new_term
	_ ->
		return ()