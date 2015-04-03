module Lab5(
			)
where

import HMM1
import Data.List 

tblHMM::HMM Char Char
tblHMM = HMM states hstates fstate rules rules2 rules3 where
		states = ['G','B','X']
		hstates = ['H','I']
		fstate = '*'
		rules = [ Rules 'H' 'X' 0.2
				 ,Rules 'H' 'B' 0.1
				 ,Rules 'H' 'G' 0.6
				 ,Rules 'I' 'X' 0.1
				 ,Rules 'I' 'B' 0.8
				 ,Rules 'I' 'G' 0.1
				]
		rules3= [ Rules '*' 'H' 0.5
				 ,Rules '*' 'I' 0.5
				 ]
		rules2= [ Rules 'H' 'I' 0.1
				 ,Rules 'I' 'H' 0.9
				 ]
		

-- нифига, недо выводить в IO :(
makeDatas::Int->HMM Char Char->[Char]->[Char]
makeDatas 0  _  res = res
makeDatas i tbl res = makeDatas (i-1) tbl res2
				where
				res2 = []

viterbi::HMM Char Char->[Char]->[Char]->[Char]
viterbi _ [] res = res
viterbi hmm (x:ended) res = viterbi hmm ended res2 where
			-- p hmm ch fst
			res2 = []
			
-- f::HMM Char Char->Char->(Char,Float)->Float
-- f hmm ch (hch,pi_1) = (get1to hmm ch)*pi_1*


