module DeBruj(
		PartDB		-- элемент графа
		,deBr		-- делает из строки граф де Брюин
		,deBrToStr	-- делает из графа строку
		,conc		-- объединит две последовательности по совпадениям (или НЕ объединит и вернёт обе)
		,giveNumOfEq	-- возвернёт длину совпадающих последовательностей по краям 
			)
where

import Data.List
import Data.Function

data PG = PG{
			 el :: PartDB
			,weighs :: Float
			}deriving(Ord, Eq,Read,Show)

data PartDB = PartDB {
			 stateFrom :: String
			,edge :: String
			,stateTo :: String
			}deriving(Ord, Eq,Read,Show)
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

conc2::(Eq a)=>[a]->[a]->[a]
conc2 g1 g2 = let
		len12 = giveNumOfEq g1 g2;
		len21 = giveNumOfEq g2 g1;
			in if (len12==0 && len21 == 0) then g1
			else if(len12>len21)
				then concat [g1,drop len12 g2]
				else concat [g2,drop len21 g1]

conc3::[PartDB]->PartDB->[PartDB]
conc3 g1 g = let
			g1To = stateTo $last g1;
			gTo = stateTo g;
			g1From= stateFrom $head g1;
			gFrom = stateFrom g;
		in if(g1To==gFrom) then g1++[g]
		else if(gTo==g1From)then [g]++g1
		else g1
-- ищем количество совпадающих последовательностей
giveNumOfEq::(Eq a)=>[a]->[a]->Int
giveNumOfEq lst1 lst2 = giveNumOfEq_ lst1 lst2 0

giveNumOfEq_::(Eq a)=>[a]->[a]->Int->Int
giveNumOfEq_ [] _ res = res
giveNumOfEq_  _ [] res = res
giveNumOfEq_ (x:lst1) (y:lst2) res = if (x==y)
					then giveNumOfEq_ lst1 lst2 (res+1)
					else res

makeKmer::[String]->Int->[PartDB]
makeKmer strs k = let
		brujs = union []$ concat $map (\x-> deBr x k) strs
		in brujs
		
-- можно ли соединить эти две последовательности
-- если можно, то b = True
-- можно, но как? сначала 1 или сначала вторая?
--
canConc2::(Eq a)=>[a]->[a]->(Bool,Int,Int)
canConc2 lst1 lst2 = (b, num, max len1 len2) where
		len1 = giveNumOfEq lst1 lst2
		len2 = giveNumOfEq lst2 lst1
		b = or [len1>0, len2>0]
		num = if len1>len2 then 0 else 1
		-- num = if len1>len2 then 1 else 2

-- можно ли добавить в путь ещё одну k-меру?
-- Bool - можно или нет
canConc::[PartDB]->PartDB->Bool -- ,Int,Int)
canConc g1 g = let
			g1To = stateTo $last g1;
			gTo = stateTo g;
			g1From= stateFrom $head g1;
			gFrom = stateFrom g;
		in (g1To==gFrom)||(gTo==g1From)
-- 
whichThreads::(Eq a)=>[a]->[[a]]->[(Bool,Int,Int)]
whichThreads lst lsts = map (\x -> canConc2 lst x) lsts


-- findWay::[PartDB]->[PartDB]->[PartDB]
findWay way lstAllEls = let
			lstBII = map (\x -> canConc way x) lstAllEls;
			-- lstB_ = filter (== True) lstBII;
			lst = map (\x -> lstAllEls !! x)(findIndices (==True) lstBII);
			in if lst == [] then way
			else maximumBy (compare `on` length) (map (\x-> (findWay (conc3 way x) (delete x lstAllEls))) lst)

-- let bruj = makeKmer ["accgt","cgtaaa","agtcc"] 3
-- let x = lab6 ["accgt","cgtaaa","agtcc"] 3			
-- lab6::[String]->Int->[PartDB]
--- deBrToStr $ lab6 ["accgt","cgtaaa","agtcc"] 3
--- казалось бы 
--- "accgtaaagtcc"
--- НО НЕТ 
--- "agtccgtaaa"
lab6 str k = let
		bruj = makeKmer str k;
	in maximumBy (compare `on` length) (map (\x -> findWay [x] (delete x bruj)) bruj)
	-- in findWay [bruj!!0] (tail bruj)
	-- in findWay [bruj!!6] (delete (bruj!!6) bruj)
			
			
			
			
			
			