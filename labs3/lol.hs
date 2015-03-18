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
			bigM = m;
			hSub = hash pat bigM; -- 2
			-- hs0 = hash (take m str) bigM;
			f::String->Int->Int->[Int]->[Int] -- 4
			f [] _ _ res = res
			f str2 hS i res = let
					hs = hash (take m str2) bigM; -- 3,8
					-- hs = hash3 str i bigM hsOld;
				in if(hS == hs) --- 5
				then if((take m $drop i str)==pat) -- 6
					then f (drop m str2) hS (i+m) (res++[i])
					else f (drop 1 str2) hS (i+1) res
				else f (drop 1 str2) hS (i+1) res

			in f str hSub 0 []

kRA2::String->String->Int
kRA2 str pat = let
			n = length str;
			m = length pat;
			hSub = hash pat m;
			-- вот так я считаю хэш, если строка сместилась на 1 символ
			hash4::Int->Int->Int
			hash4 i hsOld = let
					ch = str!!(i-1);
					ch2 = ord $str!!(i+m-1);
					hsCh = (ord ch)*(m^(m-1));
				in ((hsOld - hsCh)*m+ch2);
			-- вот так бегаем по основной строке	
			f::Int->Int->Int->Int->Int
			f i j hsOld res| i+m> n = res -- если строка уже кончилась
					| i==j+1 = let -- если смещались на 1 символ
						hs=hash4 i hsOld
						in if (catch i hs) then i
							else (f (i+1) i hs res)
					| otherwise = let -- если только начали, или сместились больше, чем на 1 символ
						hs = hash (take m$drop i str) m
						in if (catch i hs) then i
							else (f (i+1) i hs res);
			catch::Int->Int->Bool --
			catch i hs = if(hs==hSub) 
						then ((take m(drop i str)) == pat)
						else False
		in (f 0 (-2) 0 (-1))

{-
> ord 'a' 
97
> ord 'b'
98
> ord 'c'
99

(zip str [0,1..length str])
пары (Char,Int) (символ, порядковый номер)


считаем по формуле (код символа) * (какое-то простое число)^(порядковый номер)
-}
hash3::String->Int->Int->Int->Int
hash3 str i m hs = if(i<=0) then hash (take m str) m
		else if(i+m <length str) then hash2 m hs (str!!(i-1)) (str!!(i+m))		
		else hs

-- а так, если строка совсем новая		
hash::String->Int->Int
hash str m = sum $ map(\(x,y)-> (ord x)*m^y) (zip (reverse str) [0,1..length str])

hash2::Int->Int->Char->Char->Int
hash2 m hs old_a a = let
			hsA = ord a;
			hsOA = ord old_a * m^(m-1);
		in ((hs - hsOA)`div` m)+hsA

