module BioLabs2(
			bioRabbits,
			bioRabbitsDie,
			bioRabbits2
		)
where
import BioLabs1
import Data.List
import Data.Char

{-
Rabbits and Recurrence Relations http://rosalind.info/problems/fib/

Given: Positive integers n≤40 and k≤5.

Return: The total number of rabbit pairs that will be present after n months if we begin with 1 pair and in each generation, every pair of reproduction-age rabbits produces a litter of 
k rabbit pairs 
(instead of only 1 pair).

Sample Dataset
5 3
Sample Output
19
-}

bio1 :: Integer -> Integer -> Integer-> Integer -> Integer
bio1 2 _ nr nnyr = nr + nnyr
bio1 n k nr nnyr = bio1 (n-1) k (nr+nnyr) (nr*k)

bioRabbits :: Integer -> Integer->Integer
bioRabbits n k = bio1 n k 1 0

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)



{- 
Mortal Fibonacci Rabbits http://rosalind.info/problems/fibd/

Given: Positive integers n≤100 and  m≤20.
Return: The total number of pairs of rabbits that will remain after the n-th month if all rabbits live for 
m months.

Sample Dataset
6 3
Sample Output
4
-}
adding :: Integer->[Integer]->[Integer]->[Integer]
adding _ [] res  = res
adding m (el:lst0)res =if(el/=m) 
					then adding m lst0 (res ++[(el-1),m])
					else adding m lst0 (res++[el-1])


bio2 :: Integer -> Integer -> [Integer] -> Int
bio2 _ 1 lst = length(filter (>0) lst) -- 
bio2 m n lst = let 
				lst3 = adding m lst [];
			   in bio2 m (n-1) lst3

-- не работает, т.к. ОЧЕНЬ много кроликов (но идея хороша)
bioRabbitsDie :: Integer -> Integer -> Int
bioRabbitsDie n m = bio2 m n [m]



bio3 :: Integer ->Integer -> [Integer] -> Integer -> Integer
bio3 _ _ row 1 = sum row
bio3 m k row i = let 
					 p = foldl (\x y -> x+y*k) 0 (tail row);
					 row2 = (p : init row)
				 in  bio3 m k row2 (i-1)

bioRabbits2 :: Integer -> Integer-> Integer->Integer
bioRabbits2 n m k = let 
				mm = fromInteger(m-1);
				row = (1:(take mm (repeat 0)));
				in bio3 m k row n


