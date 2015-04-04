module DeBruj(
		PartDB		-- элемент графа
		,deBr		-- делает из строки граф де Брюин
		,deBrToStr	-- делает из графа строку
		,conc		-- объединит две последовательности по совпадениям (или НЕ объединит ивернёт обе)
		,giveNumOfEq	-- возвернёт длину совпадающих последовательностей по краям 
			)
where

import Data.List

data PartDB = PartDB {
			 stateFrom :: String
			,edge :: String
			,stateTo :: String
			}deriving(Eq,Read,Show)
-- let x = deBr "ACCCGT" 3
-- let y = deBr "CGTACC" 3
deBrToStr::[PartDB]->String
deBrToStr x = deBrToStr_ x []
deBrToStr_::[PartDB]->String->String
deBrToStr_ (x:[]) res = res++(edge x)
deBrToStr_ (x:dbr) res = deBrToStr_ dbr (res++(delSubBehind (edge x) (stateTo x)))

-- delSub :: String -> Int -> String -> String
delSubBehind x sub = reverse $delSub (reverse x) 1 (reverse sub)
delSub x 0 _  = x
delSub [] _ _ = []
delSub x n s | (isPrefixOf s x) =  delSub (drop p x) (n-1) s
             | otherwise = (head x) : delSub (drop 1 x) n s
               where p=length s

deBr::String->Int->[PartDB]
deBr str k = deBr_ str k []

deBr_::String->Int->[PartDB]->[PartDB]
deBr_ [] _ res = res
deBr_ str k res = let
				edg = take k str;
				stF = take (k-1) str;
				stT = drop 1 edg;
				part = PartDB stF edg stT;
				in if drop k str == [] then res++[part]
				else deBr_ (drop 1 str) k (res++[part])

-- ну, объединяем 2 последовательности
conc::(Eq a)=>[a]->[a]->[[a]]
conc g1 g2 = let
		len12 = giveNumOfEq g1 g2;
		len21 = giveNumOfEq g2 g1;
			in if (len12==0 && len21 == 0) then [g1,g2]
			else if(len12>len21)
				then [concat [g1,drop len12 g2]]
				else [concat [g2,drop len21 g1]]
-- ищем количество совпадающих последовательностей
giveNumOfEq::(Eq a)=>[a]->[a]->Int
giveNumOfEq lst1 lst2 = giveNumOfEq_ lst1 lst2 0

giveNumOfEq_::(Eq a)=>[a]->[a]->Int->Int
giveNumOfEq_ [] _ res = res
giveNumOfEq_  _ [] res = res
giveNumOfEq_ (x:lst1) (y:lst2) res = if (x==y)
					then giveNumOfEq_ lst1 lst2 (res+1)
					else res

