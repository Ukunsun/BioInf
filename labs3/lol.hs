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
			
			f::String->Int->Int->[Int]->[Int]
			f [] _ _ res = res
			f str2 hS i res = let
					hs = hash (take m str2) m; -- 3
				in if(hS == hs) --- 5
				then if((take m $drop i str)==pat) -- 6
					then f (drop m str2) hS (i+m) (res++[i])
					else f (drop 1 str2) hS (i+1) res
				else f (drop 1 str2) hS(i+1) res

			in f str hSub 0 []

{- fuckingKarpRabinAlg::String->Int-> Int->Int->[Int]->[Int]
fuckingKarpRabinAlg [] len hSub  hs res = res
fuckingKarpRabinAlg str len hSub  hs res | []
				| (length str)<=len = res
				| otherwise = let
				hs
				in if(hs==hSub)
				then 
				else fuckingKarpRabinAlg str len hSub  hs res -}

{-
> ord 'a' 
97
> ord 'b'
98
> ord 'c'
99

(zip str [0,1..length str])
фигачим пары (Char,Int) (символ, порядковый номер)

-}
hash::String->Int->Int
hash str m = sum $ map(\(x,y)-> (ord x)*m^y) (zip str [0,1..length str])


