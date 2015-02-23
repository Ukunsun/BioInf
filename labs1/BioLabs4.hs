module BioLabs4(
			mySplit,
			lcstr0,lcstr,
			dHamm
		)
where
import BioLabs1
import BioLabs2
import BioLabs3
import Data.List
import Data.Char
import Data.Ord
import Data.String.Utils
import System.IO


{-
Finding a Shared Spliced Motif http://rosalind.info/problems/lcsq/
Given: Two DNA strings s and t (each having length at most 1 kbp) in FASTA format.
Return: A longest common subsequence of s and t. (If more than one solution exists, you may return any one.)
Sample Dataset

>Rosalind_23
AACCTTGG
>Rosalind_64
ACACTGTGA

Sample Output

AACTGG

-}
 
{-
Finding a Shared Motif http://rosalind.info/problems/lcsm/

Given: A collection of k (k≤100) DNA strings of length at most 1 kbp each in FASTA format.
Return: A longest common substring of the collection. (If multiple solutions exist, you may return any single solution.)

Sample Dataset

>Rosalind_1
GATTACA
>Rosalind_2
TAGACCA
>Rosalind_3
ATACA

Sample Output
AC

lcstr "GATTACA" "TAGACCA"
"AG"
-}
 
lcstr xs ys =reverse. maximumBy (comparing length) . concat $ [f xs' ys | xs' <- tails xs] ++ [f xs ys' | ys' <- tail $ tails ys]
  where f xs ys = scanl g [] $ zip xs ys
        g z (x, y) = if x == y then x:z else []


lcstr0 xs ys = map (reverse) $union []$ filter(/="") $concat $ [f xs' ys | xs' <- tails xs] ++ [f xs ys' | ys' <- tail $ tails ys]
  where f xs ys = scanl g [] $ zip xs ys
        g z (x, y) = if x == y then x:z else []

-- нужно будет сравнивать
		
-- считаем данные из файла
{- mainSubStr :: IO() -}
mainSubStr =do
		contents <- readFile "rosalind_lcsm.txt" -- "new.txt" -- 
		let 
			lines = split "\n" contents;
			lines2 = tail$mySplit (\x -> isPrefixOf ">" x) lines [] [];
		print $vrfyItWithFire lines2
		-- считали строку, надо её разбить на подстроки
		{- let 
			gg z x = if (isPrefixOf ">" x) then z else [];
			lines = split "\n" contents
		in -} -- putStr 0 {- -}

mySplit :: ([b] -> Bool)-> [[b]]-> [[b]] -> [[b]] -> [[b]]
mySplit _ []  rr res = (res ++ [concat rr])
mySplit f (el:lst) rr res = if(f el) 
			then mySplit f lst [] (res++[concat rr])
			else mySplit f lst (rr++[el]) res


vrfy :: String -> [String]->[[String]]
vrfy str lst = map (\x -> lcstr0 x str) lst

vrfyIt:: [String] -> [String] -> String
vrfyIt strs [] = maximumBy (comparing length) strs
vrfyIt strs (el:lst)= let
				strs2 = union [] $concat$ map (\x -> lcstr0 x el) strs
			in vrfyIt strs2 lst
vrfyItWithFire :: [String] -> String
vrfyItWithFire (el:el2:lst) = vrfyIt (lcstr0 el el2) lst



{-
Counting Point Mutations http://rosalind.info/problems/hamm/

Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).
Return: The Hamming distance dH(s,t).

Sample Dataset

GAGCCTACTAACGGGAT
CATCGTAATGACGGCCT

Sample Output
7

dHamm "GAGCCTACTAACGGGAT" "CATCGTAATGACGGCCT"
7
-}


dHamm :: String -> String -> Integer --  [Integer]
dHamm xs ys =let
				g z (x,y) = if(x==y)then z else z+1;
			 in (foldl g 0 $ zip xs ys)
			 
lstHamm1 = "GCTATTCAATGCTTCTCCTGGGGGGTTGCGGCACATTCTTTCGTCGTTACCCATTATATAACACATCGATCTACTTTCGACCAGTTCCGTCAGCGAAGGCCCAGCTCTTTAGGCGTACGTGCTAACGATTTGTTGGTTTCTGATAGGCAACTAACGGCGTATCGACACTACGTGTCGACGTGGTTTGTTTACAACCTTCTCAATAAACATGCGTGTAATTGGGTATGGGAAGCGCCTTGGTCAGACCGACCCCAATTTCCGGAGCTGTATTGGTACTTGCGGCAAGAGACACCACGCACCCAGGGAAACAGTGGATCGAAGACCTGCTAATGTGGCCGTTTGACACTCTATATAGTAGACAAAACCGTCCCATCTTAGTCTCTCCAATTAGCTTGTAGCGACGGTTTTAACCCCCGCTTAACCCTCCATGTGGGAACCAGAAGTTCCAGATCTGCACTCTCGCACAGGGTTACAAAGCACTCCATGGGGGTGGGGCCTGGTCTGACTCCCTCGCCTTATCAGGATCTAGACTCACATGAGAGCGAAGCGGAATTGTGTTCGTATCTGCCCGTATTGCAATCAGCCTTACTTTCGCTCAGTCGCCACCCCCGTCGAAGTCTTGCCTTATGTAGTTCGTTTTACTTCCCACCGCTCATAATGAATCTTCCTTCGCAATCTGAGCAGCCATGAATAGGGACTTCTACATTTACCTTACGTCGAGCGGCCAGGTACTCAAGAAAGTCCTCTGAGATACGTGATAAGAGTGGTGTGCGCTTCCCCATACGCGCCAATGACGTCTGGGCCTCCGTCCGATATAGGGAAAATTAACGGAGCTCCTCTACTCCTTCACCCTAGTCGGGACCCCTCTGTCCCGTAAGCTTTCGACAGGCGTGCGTGGTACCTCAAACCAGAACATCCCGTTGAACTCTGCCGAAATTGTTGTTCGGTTGGCTATGGTACGTTAAAGGGGATTCTGCACAATGGTCCTA"
lstHamm2 = "CCAGCTCGATGAATTCAATTGGTGATGGCTTAATCTGATATCGATGGAGCTTGTGCCGATACAAATAGCTCGACCAACGGGCACCTCCGTTTCCGAATGGCCGGGACTGAAGTCGCATAGACTGAATACTTGCTGCAGAGCGCCGTAACACTAGGGCGGAATGTTCACAGCTTGTAGACGCGGCACAACCACGAGACCGACAGTATGTAGACGCGTAGATGTAGAGGCGACTGAGTCTGAGCTCCAAGTCACCTATCCCTGGAGCAGGCTGGGTTCTTAGGCCAGACCACGGTGCGAACAAACGCGCAAACGGGTTCGAAGTGCAGCTCAGGTAGGCGTTTAACCCCTCTTAGTGTAATATGAACCATCACATCGAAGGAACTCGCCCCATATCGTTGCGATTGATCTAATACCTGGTTAGCCGCCTACACTTCGGTCATAGCTTCTAGTCCTCCCCGCTCGTTACTGGCTATCCTTGGGCACATCGCCGGGGCGCCGGATTTGACTCGGTCGGCTCAAAAGGTGTAGGTCGAGTGTCCGCTAGTAAAGGGGTTGTGGAAGGATCTGTACCTTTTGCAACTGTCGTGCGGTCCGCGCTGTCCAGCATCAAATAGTATTTTTGCCCTATTGACTTTGGGTATGTTAGGACGGTTCAGACTCAGTGCGCCTTTACCGTCTGAGTTGTCATGAAGAAGGTTTACGTAACGCACCTGATGGTGTCTAGTTAGGACTGGAAGTACTAGCTCTGCATTTGGTGGCACTAGTGGTGAGTGTCTCACTCTACTATCGGCTCCTTACTGGGCTGCTGTCCGATAGAGCCAAGTTCGACAGACATCCGATAATGGTTTACGGTGCTCGGGACCACACTAAGCCGGACACTTACGACTGGCCGCATACGTAACTATCAGTTGCACGGCCACTCAACCTCACTTGAAACCGTACGACGGATCACTTTGATGCCACATAGGGTCTTTGGAACCACGGTACTC"

