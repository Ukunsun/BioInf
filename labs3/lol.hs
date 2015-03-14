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

kRA::String->String->[Int]
kRA str pat = let
			m = length pat;
			hSub = hash pat m;
			
			f::String->Int->Int->Int->[Int]->[Int]
			f [] _ _ _ res = res
			f str2 hS hs0 i res = let
					hs = hash (take m str2) m;
				in if(hS == hs)
				then if((take m $drop i str)==pat)
					then f (drop m str2) hS hs (i+m) (res++[i])
					else f (drop 1 str2) hS hs (i+1) res
				else f (drop 1 str2) hS hs (i+1) res
			
			in f str hSub 0 0 []

{- fuckingKarpRabinAlg::String->Int-> Int->Int->[Int]->[Int]
fuckingKarpRabinAlg [] len hSub  hs res = res
fuckingKarpRabinAlg str len hSub  hs res | []
				| (length str)<=len = res
				| otherwise = let
				hs
				in if(hs==hSub)
				then 
				else fuckingKarpRabinAlg str len hSub  hs res -}


hash::String->Int->Int
hash str m = sum $ map(\(x,y)-> (ord x)*m^y) (zip str [0,1..length str])