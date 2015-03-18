import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString.Char8 as BS
	 
{- levenstein::BS.ByteString->BS.ByteString->Int
levenstein s1 s2 = let
			transform::BS.ByteString->[Int]->Char->Int
			transform str xs@(x:xs') c = let
				compute c' x y z = minimum [y+1, z+1, x+ if c' == c then 0 else 1]
				res = x+1: zipWith4 compute str xs xs' res
				in res
		in last $ BS.foldl (transform s1) [0.. BS.length s1] s2 -}

type LevTable = V.Vector(V.Vector Int)

levenTable::BS.ByteString->BS.ByteString->LevTable
levenTable str1 str2 = let
			m = BS.length str1;
			n = BS.length str2;
			--  create (do { v <- new 2; write v 0 'a'; write v 1 'b' }) = <a,b>
			res = V.fromList $take n (repeat $V.fromList [0..m]);
			-- makeTbl::Int->Int->
			-- makeTbl i j  | i==0 && j==0 = do VM.write res V.! i V.!j
		in res -- V.create $ do
			-- p <- VM.new n
			-- VM.set p 0 -}

{-
let s1 = BS.pack "ABCDEFGH"
let s2 = BS.pack "ACDEXGIH"

getLev2 ss1 ss2
getLev "ABCDEFGH" "ACDEXGIH"
-}
ss1 = BS.pack "ABCDEFGH"
ss2 = BS.pack "ACDEXGIH"

getLev::String->String->Int
getLev str1 str2 = V.last $V.last $ levenTbl (BS.pack str1) (BS.pack str2)

getLev2::BS.ByteString->BS.ByteString->Int
getLev2 str1 str2 = V.last $V.last $ levenTbl str1 str2

levenTbl::BS.ByteString->BS.ByteString->LevTable
levenTbl str1 str2 = levTbl 0 str1 str2 res where
			res = V.fromList[]

levTbl::Int->BS.ByteString->BS.ByteString->LevTable->LevTable
levTbl ii str1 str2 res = let
			m = BS.length str1; -- j
			n = BS.length str2; -- i
			levTbl_::Int->Int->V.Vector Int
			levTbl_ i j_ = V.create $do
						p <- VM.new m
						VM.set p 0
						let loop j | j == m = return ()
								-- | (j == 0 && i==0) = do
								--	VM.write p 0 0
								--		loop (j+1) 
								| (i>=0 && j == 0) = do
										VM.write p j i
										loop (j+1)
								| (j>0 && (i == 0)) = do
										VM.write p j j
										loop (j+1)
								| otherwise = let
										mB a b=if( a==b) then 0 else 1;
										m1 = mB (BS.index str1 i) (BS.index str2 j);
										-- dij_1 <- (VM.unsafeRead  p (j-1));
										di_1j = ((res V.! (i-1)) V.! j)+1;
										di_1j_1 = ((res V.! (i-1)) V.! (j-1))+m1;
										
										-- mm = minimum [dij_1+1, di_1j,di_1j_1]
										in do
										dd <-VM.unsafeRead  p (j-1)
										VM.write p j (minimum [dd+1,di_1j,di_1j_1])
										loop (j+1)
						loop j_
						return p
		in  if(ii == n) then res
			else levTbl (ii+1) str1 str2 (V.snoc res (levTbl_ ii 0))




