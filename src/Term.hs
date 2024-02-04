{-# LANGUAGE LambdaCase #-}
module Term where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Util (try_parens, UniqueNames, go, ShowParens, show_parens)
import qualified DistanceExtension
import qualified ListExtension
import qualified RegionExtension

-- 2.1.1
data Term
	= Refer String
	| Supply Term Term
	| Assume String Term
	-- 3.1
	| DistanceExtension (DistanceExtension.Term Term)
	| ListExtension (ListExtension.Term Term)
	| RegionExtension (RegionExtension.Term Term)
	| Define String Term Term

get_assume :: Term -> Maybe (String, Term)
get_assume = \case
	Assume name usage -> Just (name, usage)
	_ -> Nothing

get_supply :: Term -> Maybe (Term, Term)
get_supply = \case
	Supply process input -> Just (process, input)
	_ -> Nothing

get_distance_extension :: Term -> Maybe (DistanceExtension.Term Term)
get_distance_extension = \case
	DistanceExtension extension -> Just extension
	_ -> Nothing

get_list_extension :: Term -> Maybe (ListExtension.Term Term)
get_list_extension = \case
	ListExtension extension -> Just extension
	_ -> Nothing

get_region_extension :: Term -> Maybe (RegionExtension.Term Term)
get_region_extension = \case
	RegionExtension extension -> Just extension
	_ -> Nothing

instance Show Term where
	show = show_parens True

instance ShowParens Term where
	show_parens should = \case
		-- 2.1.2
		Refer name ->
			name
		Supply process input ->
			try_parens should [show_parens True process, " ", show_parens True input]
		Assume name usage ->
			try_parens should [name, ": ", show_parens False usage]
		-- 3.1
		DistanceExtension extension ->
			show_parens should extension
		ListExtension extension ->
			show_parens should extension
		RegionExtension extension ->
			show_parens should extension
		Define name definition usage ->
			try_parens should ["@", name, " := ", show_parens True definition, " in ", show_parens False usage]

-- 2.2.1
instance UniqueNames Term where
	go bound = \case
		Refer name ->
			Refer $ fromMaybe name (Map.lookup name bound)
		Supply process input ->
			Supply (go bound process) (go bound input)
		Assume name usage ->
			Assume new_name (go (Map.insert name new_name bound) usage)
			where
			new_name = find_unique bound name
		-- 3.1
		DistanceExtension extension ->
			DistanceExtension (go bound extension)
		ListExtension extension ->
			ListExtension (go bound extension)
		RegionExtension extension ->
			RegionExtension (go bound extension)
		Define name definition usage ->
			Define new_name (go new_bound definition) (go new_bound usage)
			where
			new_bound = Map.insert name new_name bound
			new_name = find_unique bound name

unique_names :: Term -> Term
unique_names = go Map.empty

find_unique :: Map.Map String a -> String -> String
find_unique taken name =
	head $ filter (`Map.notMember` taken) (iterate (++ "0") name)