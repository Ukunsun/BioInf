module HMM1(
			HMM
			,Rules
			,get1to
			,get2to
			,get3to
			)
where

import Data.List


data Rules hStateType stateType = Rules{ from :: hStateType
				, to :: stateType
				, cost :: Float
				}deriving(Eq,Read,Show)

data HMM stateType hStateType= HMM { states ::[stateType]
			,hstates::[hStateType]
			,fstate :: stateType
			, rules ::[Rules hStateType stateType]
			, rules2::[Rules hStateType hStateType]
			, rules3::[Rules stateType hStateType]
				}deriving(Eq,Read,Show)

get3to::Eq hStateType => Eq stateType => HMM stateType hStateType->hStateType->[Rules stateType hStateType]
get3to hmm hst = let 
				-- f::[Rules stateType hStateType]->hStateType->[Rules stateType hStateType]
				f [] _ = []
				f (r:rs) s = let
								s1 = to r;
							in if (s1 == s) 
							 then [r] 
							 else f rs s
			in f (rules3 hmm) hst

get2to::Eq hStateType => HMM hStateType hStateType->hStateType->hStateType->[Rules hStateType hStateType]
get2to hmm tost fromst = let 
				-- f::[Rules hStateType hStateType]->hStateType->hStateType->Rules hStateType hStateType
				f [] _  _ = []
				f (r:rs) tos fs = if (to r == tos)&&(from r == fs) then [r] else f rs tos fs
			in f (rules2 hmm) tost fromst

get1to::Eq hStateType => Eq stateType => HMM stateType hStateType->stateType->hStateType->[Rules hStateType stateType]
get1to hmm tost fst = let
				-- f::[Rules hStateType stateType]->stateType->hStateType->Rules hStateType stateType
				f [] _  _ = []
				f (r:rs) tos fs = if (to r==tos)&&(from r == fs) then [r] else f rs tos fs
			in f (rules hmm) tost fst


			
-- x - скрытое состояние
-- y - явное
perh::HMM Char Char -> Char -> Char -> Float
perh hmm y x = cost.head$get1to hmm y x

perh3::HMM Char Char -> Char -> Float
perh3 hmm x = cost.head$get3to hmm x
-- get2to hmm y x; -- получили правило перехода из скрытого состояния x в y

perhs hmm (x:[]) = maximum $ map (\el -> ((perh hmm x el)* (perh3 hmm el),el)) (hstates hmm)
perhs hmm (x:str) = maximum $ map (\el -> let (a,b) = (perhs hmm str) in((perh hmm x el)*a,el)) (hstates hmm)


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