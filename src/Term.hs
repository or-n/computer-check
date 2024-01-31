{-# LANGUAGE LambdaCase #-}
module Term where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Util (parens)

-- 2.1.1
data Term
	= Refer String
	| Supply Term Term
	| Assume String Term

get_assume :: Term -> Maybe (String, Term)
get_assume = \case
	Assume name usage -> Just (name, usage)
	_ -> Nothing

get_supply :: Term -> Maybe (Term, Term)
get_supply = \case
	Supply process input -> Just (process, input)
	_ -> Nothing

-- 2.1.2
instance Show Term where
	show = go True where
		go in_parens = \case
			Refer name ->
				name
			Supply process input ->
				parens in_parens $ concat [go True process, " ", go True input]
			Assume name usage ->
				parens in_parens $ concat [name, ": ", go False usage]

-- 2.2.1
unique_names :: Term -> Term
unique_names = go Map.empty where
	go bound = \case
		Refer name ->
			Refer $ fromMaybe name (Map.lookup name bound)
		Supply process input ->
			Supply (go bound process) (go bound input)
		Assume name usage ->
			Assume new_name (go (Map.insert name new_name bound) usage)
			where
			new_name = find_unique bound name

find_unique :: Map.Map String a -> String -> String
find_unique taken name =
	head $ filter (`Map.notMember` taken) (iterate (++ "0") name)