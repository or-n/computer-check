{-# LANGUAGE LambdaCase, MultiParamTypeClasses #-}
module Run where

import Term
import TermType (TermType(..))
import qualified DistanceExtension
import qualified ListExtension
import qualified RegionExtension
import Util (Substitute, substitute)
import qualified Data.Map as Map

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
		RegionExtension extension -> RegionExtension $
			substitute substituted_name term extension
		Define name definition usage | name /= substituted_name ->
			Define name (go definition) (go usage)
		x -> x
		where go = substitute substituted_name term

type State = Map.Map Int Term

-- 2.2.3
run_strict :: State -> Term -> (State, Term)
run_strict state term = maybe (state, term) (uncurry run_strict) (strict_step state term)

-- 2.2.3

strict_step :: State -> Term -> Maybe (State, Term)
strict_step state term =
	case term of
		Supply process input -> case strict_step state input of
			Just (new_state, input') ->
				Just (new_state, Supply process input')
			_ -> do
				let (new_state, process') = run_strict state process
				(name, usage) <- get_assume process'
				Just (new_state, substitute name input usage)
		DistanceExtension extension -> do
			let value x = get_distance_extension x
				>>= DistanceExtension.get_value
			case extension of
				DistanceExtension.Add a b -> do
					let (state2, a') = run_strict state a
					a_value <- value a'
					let (state3, b') = run_strict state2 b
					b_value <- value b'
					let v = DistanceExtension.Value (a_value + b_value)
					Just (state3, DistanceExtension v)
				DistanceExtension.Sub a b -> do
					let (state2, a') = run_strict state a
					a_value <- value a'
					let (state3, b') = run_strict state2 b
					b_value <- value b'
					let v = DistanceExtension.Value (a_value - b_value)
					Just (state3, DistanceExtension v)
				DistanceExtension.IfZero x yes no ->
					case strict_step state x of
						Just (new_state, x') -> do
							let v = DistanceExtension.IfZero x' yes no
							Just (new_state, DistanceExtension v)
						_ -> do
							x_value <- value x
							Just (state, if x_value == 0 then yes else no)
				_ ->
					Nothing
		ListExtension extension -> do
			let push x = get_list_extension x
				>>= ListExtension.get_push
			case extension of
				ListExtension.Top pair -> do
					case strict_step state pair of
						Just (new_state, pair') ->
							Just (new_state, ListExtension (ListExtension.Top pair'))
						_ -> do
							(top, _) <- push pair
							Just (state, top)
				ListExtension.Rest pair -> do
					case strict_step state pair of
						Just (new_state, pair') ->
							Just (new_state, ListExtension (ListExtension.Rest pair'))
						_ -> do
							(_, rest) <- push pair
							Just (state, rest)
				ListExtension.IfEmpty x yes no ->
					case strict_step state x of
						Just (new_state, x') -> do
							let v = ListExtension.IfEmpty x' yes no
							Just (new_state, ListExtension v)
						_ -> do
							list_extension <- get_list_extension x
							case list_extension of
								ListExtension.End ->
									Just (state, yes)
								ListExtension.Push _ _ ->
									Just (state, no)
								_ ->
									Nothing
				_ ->
					Nothing
		RegionExtension extension -> do
			let get x = get_region_extension x
				>>= RegionExtension.get_region
			case extension of
				RegionExtension.Reference value -> do
					let (state2, value') = run_strict state value
					let index = Map.size state2
					let state3 = Map.insert index value' state2
					Just (state3, RegionExtension (RegionExtension.Region index))
				RegionExtension.Dereference region -> do
					let (state2, region') = run_strict state region
					index <- get region'
					value <- Map.lookup index state2
					Just (state2, value)
				RegionExtension.Assign region value -> do
					let (state2, region') = run_strict state region
					index <- get region'
					let (state3, value') = run_strict state2 value
					let state4 = Map.insert index value' state3
					Just (state4, ListExtension ListExtension.End)
				RegionExtension.Region index ->
					Nothing
		Define name definition usage -> do
			let (new_state, v) = run_strict state definition
			let v' = substitute name (Define name definition (Refer name)) v
			Just (new_state, substitute name v' usage)
		_ ->
			Nothing

-- 2.2.4
print_strict_steps :: Term -> IO ()
print_strict_steps = go Map.empty where
	go state term = case strict_step state term of
		Just (new_state, new_term) -> do
			print new_term
			go new_state new_term
		_ ->
			return ()