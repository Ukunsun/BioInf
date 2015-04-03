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

hmm1 = tblHMM
-- lab5 hmm1 "GGXB"
-- [(0.3,'H'),(0.18,'H'),(3.6000002e-2,'H'),(2.8800001e-2,'I')]
lab5 hmm str = reverse$map (\x -> perhs hmm x) (init$tails$reverse str)

