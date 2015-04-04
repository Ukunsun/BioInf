module DeBruj(
		PartDB
		,deBr
			)
where

import Data.List

data PartDB = PartDB {
			 stateFrom :: String
			,edge :: String
			,stateTo :: String
			}deriving(Eq,Read,Show)

deBr::String->Int->[PartDB]->[PartDB]
deBr [] _ res = res
deBr str k res = let
				edg = take k str;
				stF = take (k-1) str;
				stT = drop 1 edg;
				part = PartDB stF edg stT;
				in deBr (drop 1 str) k (res++[part])