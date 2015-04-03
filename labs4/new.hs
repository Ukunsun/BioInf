import Data.List
ss1 = "ABCDEFGH"
ss2 = "ACDEXGIH"

-- > tblLeven 0 "ABCDEFGH" "ACDEXGIH" []
-- 3
tblLeven::Int->String->String->[[Int]]->[[Int]]
tblLeven i_ str1 str2 maxRes= let
			n = length str1;
			m = length str2;
			ttblLev::Int->Int->[Int]->[Int]
			ttblLev i j res |j>=m = res
						| i>=0&&j==0 = ttblLev i (j+1) (res++[i])
						| i==0&&j>0 = ttblLev i (j+1) (res++[j])
						|otherwise = let
							m a b = if(a==b) then 0 else 1;
							m1 = m (str1!!i) (str2!!j)
							mm = minimum[(res!!(j-1)) +1,
										(maxRes!!(i-1)!!j) +1,
										(maxRes!!(i-1)!!(j-1)) +m1]
						in ttblLev i (j+1) (res ++ [mm])
			in if(i_ >= n)
			then maxRes
			else tblLeven (i_+1) str1 str2 (maxRes++[ttblLev i_ 0 []])

myLev str1 str2 = last $ last$ tblLeven 0 str1 str2 []					
{-
> levenshtein "ABCDEFGH" "ACDEXGIH"
3
-}
levenshtein s1 s2 = last $ foldl (transform s1) [0..length s1] s2 where
         transform str xs@(x:xs') c = res where
                 res = x + 1 : zipWith4 compute str xs xs' res
                 compute c' x y z = minimum [y + 1, z + 1, x + if c' == c then 0 else 1]
