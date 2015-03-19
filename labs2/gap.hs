-- http://www.avatar.se/molbioinfo2001/dynprog/adv_dynamic.html 
import System.IO
import Data.Matrix
import Data.List
import Data.String.Utils

type Matr = Matrix Int



getEl::(Char,Char)->(Matr,String)->Int
getEl (ch1,ch2) (matr,str) = let
				i = head $findIndices (==ch1) str;
				j = head $findIndices (==ch2) str
			in (!) matr (i,j) where

affineGap::String->String->Int->Int->(Matr,[Char])->Matr
affineGap str1 str2 wg ws blosum= let
				m = length str1;
				n = length str2;
				firstWork::Int->[Matr]->[Matr]
				firstWork i res@[a,b] | i>m = res
						 |otherwise = let
								w1 = wg+i*ws;
							in firstWork (i+1) [setElem w1 (i,1) a,setElem w1 (i,1) b]
				secondWork::Int->[Matr]->[Matr]
				secondWork i res@[a,b] | i>n = res
					 |otherwise = let
							w1 = wg+ i*ws;
						in secondWork (i+1) [setElem w1 (1,i) a,setElem w1 (1,i) b]
				
				mainWork::Int->Int->[Matr]->[Matr]
				mainWork i j res@[g_,e_,f_,v_] | i>m = res
							-- | i==3 && j>3 = res
							|j>n = mainWork (i+1) 2 res
							| otherwise = let
									nums = (str1!!(i-1),str2!!(j-1));
									vi1j1 = (!)v_ (i-1,j-1);
									gEl = vi1j1+(getEl nums blosum);
									_g = setElem gEl (i,j) g_;
									eij1=(!)e_ (i,j-1);
									vij1=(!)v_(i,j-1);
									eEl = maximum[eij1+ws, vij1+wg];
									_e = setElem eEl (i,j) e_;
									fi1j=(!)f_(i-1,j);
									vi1j=(!)v_(i-1,j);
									fEl = maximum[fi1j+ws,vi1j+wg];
									_f = setElem fEl (i,j) f_;
									
									vEl = maximum[(!)_e(i,j),(!)_f(i,j),(!)_g(i,j)];
									_v = setElem vEl (i,j) v_;
								in mainWork i (j+1) [_g,_e,_f,_v];
				[vv,ee] = firstWork 2 (take 2(repeat (zero m n)));
				[v1,e1] = secondWork 2 [vv,ee];
			    -- [v0,e0] = secondWork 2 $ firstWork 2 (take 2(repeat (zero m n)));
				zer = zero m n;
				[g,e,f,v] = mainWork 2 2 [zer, e1, zer, v1];
			in v --  [v,e]-- [g,e,f,v]

scoringMatrix::String->String->(Matr,String)->Matr
scoringMatrix str1 str2 blosum = let
				m = length str1;
				n = length str2;
				-- d0 = zero m n;
				firstWork::Int->Matr->Matr
				firstWork i res | i>m = res
						 |otherwise = firstWork (i+1) (setElem i (i,1) res)
				secondWork::Int->Matr->Matr
				secondWork i res | i>n = res
					 |otherwise = secondWork (i+1) (setElem i (1,i) res)
				mainWork::Int->Int->Matr->Matr
				mainWork i j res | i>m = res
						|j>n = mainWork (i+1) 2 res
						| otherwise = let
								dd = maximum [((!)res (i-1,j))
										+(getEl (str1!!(i-1),'*') blosum),
										((!)res (i, j-1))
										+(getEl ('*',str2!!(j-1)) blosum),
										((!)res (i-1, j-1))
										+(getEl (str1!!(i-1),str2!!(j-1)) blosum)]
							in mainWork i (j+1) (setElem dd (i,j) res);
				d0 = firstWork 2 (zero m n);
				d1 = secondWork 2 d0;
				d = mainWork 2 2 d1;
			in d
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Mutable as VM
-- import qualified Data.ByteString.Char8 as BS

-- type VecMatr = V.Vector(V.Vector Int)

-- toMatrWrite::VecMatr->Int->Int->Int->VecMatr
-- toMatrWrite matr i j el = let
			-- x = matr V.! i;
			-- res = VM.write x j el
		-- in VM.write matr i res 
-- i=1::Int
-- j=2::Int
-- VM.write matr i $ VM.write (matr V.! i) j el
{- createMatr::Int->Int->Int->VecMatr
createMatr m n el = let
				matr = take m $ repeat $take n $repeat el;
			in V.fromList $ map(\x->V.fromList x) matr -}

-- создать вспомогательные матрицы
{- afGap::String->String->Int->Int->VecMatr
afGap str1 str2 wg ws= let
			m = length str1;
			n = length str2;
			v = createMatr m n 0;
			g = createMatr m n 0;
			e = createMatr m n 0;
			f = createMatr m n 0;
			ij0::V.Vector Int
			ij0 =V.create $do
					p <- VM.new m
					VM.set p 0
					let loop i | i>=m = return()
							|otherwise = do 
								VM.write p i (wg + (i-1)*ws)
								loop (i+1)
					in loop 0
			i0j = V.create $do
					p<-V.new n
		in []

-}
blso =(fromList 24 24[4,-1,-2,-2,0,-1,-1,0,-2,-1,-1,-1,-1,-2,-1,1,0,-3,-2,0,-2,-1,0,-4,-1,5,0,-2,-3,1,0,-2,0,-3,-2,2,-1,-3,-2,-1,-1,-3,-2,-3,-1,0,-1,-4,-2,0,6,1,-3,0,0,0,1,-3,-3,0,-2,-3,-2,1,0,-4,-2,-3,3,0,-1,-4,-2,-2,1,6,-3,0,2,-1,-1,-3,-4,-1,-3,-3,-1,0,-1,-4,-3,-3,4,1,-1,-4,0,-3,-3,-3,9,-3,-4,-3,-3,-1,-1,-3,-1,-2,-3,-1,-1,-2,-2,-1,-3,-3,-2,-4,-1,1,0,0,-3,5,2,-2,0,-3,-2,1,0,-3,-1,0,-1,-2,-1,-2,0,3,-1,-4,-1,0,0,2,-4,2,5,-2,0,-3,-3,1,-2,-3,-1,0,-1,-3,-2,-2,1,4,-1,-4,0,-2,0,-1,-3,-2,-2,6,-2,-4,-4,-2,-3,-3,-2,0,-2,-2,-3,-3,-1,-2,-1,-4,-2,0,1,-1,-3,0,0,-2,8,-3,-3,-1,-2,-1,-2,-1,-2,-2,2,-3,0,0,-1,-4,-1,-3,-3,-3,-1,-3,-3,-4,-3,4,2,-3,1,0,-3,-2,-1,-3,-1,3,-3,-3,-1,-4,-1,-2,-3,-4,-1,-2,-3,-4,-3,2,4,-2,2,0,-3,-2,-1,-2,-1,1,-4,-3,-1,-4,-1,2,0,-1,-3,1,1,-2,-1,-3,-2,5,-1,-3,-1,0,-1,-3,-2,-2,0,1,-1,-4,-1,-1,-2,-3,-1,0,-2,-3,-2,1,2,-1,5,0,-2,-1,-1,-1,-1,1,-3,-1,-1,-4,-2,-3,-3,-3,-2,-3,-3,-3,-1,0,0,-3,0,6,-4,-2,-2,1,3,-1,-3,-3,-1,-4,-1,-2,-2,-1,-3,-1,-1,-2,-2,-3,-3,-1,-2,-4,7,-1,-1,-4,-3,-2,-2,-1,-2,-4,1,-1,1,0,-1,0,0,0,-1,-2,-2,0,-1,-2,-1,4,1,-3,-2,-2,0,0,0,-4,0,-1,0,-1,-1,-1,-1,-2,-2,-1,-1,-1,-1,-2,-1,1,5,-2,-2,0,-1,-1,0,-4,-3,-3,-4,-4,-2,-2,-3,-2,-2,-3,-2,-3,-1,1,-4,-3,-2,11,2,-3,-4,-3,-2,-4,-2,-2,-2,-3,-2,-1,-2,-3,2,-1,-1,-2,-1,3,-3,-2,-2,2,7,-1,-3,-2,-1,-4,0,-3,-3,-3,-1,-2,-2,-3,-3,3,1,-2,1,-1,-2,-2,0,-3,-1,4,-3,-2,-1,-4,-2,-1,3,4,-3,0,1,-1,0,-3,-4,0,-3,-3,-2,0,-1,-4,-3,-3,4,1,-1,-4,-1,0,0,1,-3,3,4,-2,0,-3,-3,1,-1,-3,-1,0,-1,-3,-2,-2,1,4,-1,-4,0,-1,-1,-1,-2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-2,0,0,-2,-1,-1,-1,-1,-1,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,-4,1]::Matrix Int,"ARNDCQEGHILKMFPSTWYVBZX*")
-- affineGap "PRTEINS" "PRTWPSEIN" (-11) (-1) blso

main=do
	matrBLOSUM <-readFile "BLOSUM.txt"
	let matrBL = split "\n" matrBLOSUM
	let str = concat $tail(split " " $head matrBL)
	let matr = map (\x -> (map (\y-> read y::Int)(split " " $drop 2 x))) (tail matrBL)
	let len = length matr
	let blosum = (fromList len len (concat matr), str)
	let str1 = "PRTEINS"
	let str2 = "PRTWPSEIN"
	-- let str1 = "GAATTCAGTTA"
	-- let str2 = "GGATCGA"
	
	print "AffineGAP"
	print $affineGap str1 str2 (0) (1) blosum -- (-11) (-1) blosum
	print "scoringMatrix"
	print $scoringMatrix str1 str2 blosum