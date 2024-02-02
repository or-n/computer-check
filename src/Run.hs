{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}
module Run where

import Term
import qualified DistanceExtension
import qualified ListExtension
import Util (Substitute, substitute)

-- 2.2.2
instance Substitute Term Term where
	substitute substituted_name term = \case
		Refer name | name == substituted_name ->
			term
		Supply process input ->
			Supply (go process) (go input)
		Assume name usage | name /= substituted_name ->
			Assume name (go usage)
		DistanceExtension extension -> DistanceExtension $
			substitute substituted_name term extension
		ListExtension extension -> ListExtension $
			substitute substituted_name term extension
		Define name definition usage | name /= substituted_name ->
			Define name (go definition) (go usage)
		x -> x
		where go = substitute substituted_name term

-- 2.2.3
run_strict :: Term -> Term
run_strict term = maybe term run_strict (strict_step term)

-- 2.2.3
strict_step :: Term -> Maybe Term
strict_step term =
	case term of
		Supply process input -> case strict_step input of
			Just input' ->
				Just (Supply process input')
			_ -> do
				(name, usage) <- get_assume (run_strict process)
				Just (substitute name input usage)
		DistanceExtension extension -> do
			let value x = get_distance_extension x
				>>= DistanceExtension.get_value
			case extension of
				DistanceExtension.Add a b -> do
					a_value <- value (run_strict a)
					b_value <- value (run_strict b)
					let v = DistanceExtension.Value (a_value + b_value)
					Just (DistanceExtension v)
				DistanceExtension.Sub a b -> do
					a_value <- value (run_strict a)
					b_value <- value (run_strict b)
					let v = DistanceExtension.Value (a_value - b_value)
					Just (DistanceExtension v)
				DistanceExtension.IfZero x yes no ->
					case strict_step x of
						Just x' -> do
							let v = DistanceExtension.IfZero x' yes no
							Just (DistanceExtension v)
						_ -> do
							x_value <- value x
							Just (if x_value == 0 then yes else no)
				_ ->
					Nothing
		ListExtension extension -> do
			let push x = get_list_extension x
				>>= ListExtension.get_push
			case extension of
				ListExtension.Top pair -> do
					case strict_step pair of
						Just pair' ->
							Just (ListExtension (ListExtension.Top pair'))
						_ -> do
							(top, _) <- push pair
							Just top
				ListExtension.Rest pair -> do
					case strict_step pair of
						Just pair' ->
							Just (ListExtension (ListExtension.Top pair'))
						_ -> do
							(_, rest) <- push pair
							Just rest
				ListExtension.IfEmpty x yes no ->
					case strict_step x of
						Just x' -> do
							let v = ListExtension.IfEmpty x' yes no
							Just (ListExtension v)
						_ -> do
							extension <- get_list_extension x
							case extension of
								ListExtension.End -> Just yes
								ListExtension.Push _ _ -> Just no
								_ -> Nothing
				_ ->
					Nothing
		Define name definition usage -> do
			let v = run_strict definition
			let v' = substitute name (Define name definition (Refer name)) v
			Just (substitute name v' usage)
		_ ->
			Nothing

-- 2.2.4
print_strict_steps :: Term -> IO ()
print_strict_steps term = case strict_step term of
	Just new_term -> do
		print new_term
		print_strict_steps new_term
	_ ->
		return ()