import Data.List
import Data.Char

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)   
					 
nativeSearch::String->String->Int->[Int]->[Int]
nativeSearch [] _ _ res = res
nativeSearch (el:str) pat i res = let
					len = length pat;
					str2 = [el]++(take (len - 1) str);
					in if(str2 == pat) 
					then nativeSearch (drop (len-1) str) pat (i+len)(res++[i])
					else nativeSearch str pat (i+1) res

 {- 
 1 function RabinKarp(string s[1..n], string sub[1..m])
 2     hsub := hash(sub[1..m])
 3     hs := hash(s[1..m])
 4     for i from 1 to (n-m+1)
 5         if hs = hsub
 6             if s[i..i+m-1] = sub
 7                 return i
 8         hs := hash(s[i+1..i+m])
 9     return not found
 -}
kRA::String->String->[Int]
kRA str pat = let
			m = length pat;
			hSub = hash pat m; -- 2
			
			f::String->Int->Int->[Int]->[Int] -- 4
			f [] _ _ res = res
			f str2 hS i res = let
					hs = hash (take m str2) m; -- 3,8
				in if(hS == hs) --- 5
				then if((take m $drop i str)==pat) -- 6
					then f (drop m str2) hS (i+m) (res++[i])
					else f (drop 1 str2) hS (i+1) res
				else f (drop 1 str2) hS(i+1) res

			in f str hSub 0 []

{-
> ord 'a' 
97
> ord 'b'
98
> ord 'c'
99

(zip str [0,1..length str])
пары (Char,Int) (символ, порядковый номер)

-}
hash::String->Int->Int
hash str m = sum $ map(\(x,y)-> (ord x)*m^y) (zip str [0,1..length str])



{- bm::String->String->Int
bm str pat = let
			tbl = []
		in 0
checkHigh::String->String->Int->Int
checkHigh str pat m = if(str!!m == (last pat))
		then 0
		else checkInside str pat m

checkInside::String->String->Int->Int
checkInside str pat m = let
			el = str!!m;
			-- num = элементу из таблицы смещений
			-- num = get tbl el;
			num = last $findIndices (== el) str;
			in if (elem el pat) 
			then checkHigh str pat (m+num) -- checkFromNum str pat m num;
			else checkHigh str pat (m + length pat)

-- checkFromNum::String->String->Int->Int->
-- checkFromNum str pat m num

get::[(Char,Int)]->Char->Int
get [] el = -1
get ((x,y):lst) el = if (x == el)
			then y
			else get lst el
			
checkEl::String->String->Int->Int
checkEl str pat m =  let
		m = length pat;
		elM = str!!m;
		elP = str!!m;
		in if(elM==elP)
		then 0
		else 0 -}