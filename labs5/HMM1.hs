module HMM1(
			 Rules(..)
			,HMM(..)
			,get1to		-- вернуть правило из rules,  которое ведёт В указанное состояние ИЗ указанного
			,get2to		-- вернуть правило из rules2, которое ведёт В указанное состояние ИЗ указанного
			,get3to		-- вернуть правило из rules3, которое ведёт В указанное состояние
			,perh		-- вернуть вероятность перехода В указанное ИЗ указаннного состояния по таблице правил rules
			,perh2
			,perh3		-- вернуть вероятность перехода В указанное ИЗ указаннного состояния по таблице правил rules3
			,perhs
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

get2to::Eq hStateType => HMM stateType hStateType->hStateType->hStateType->[Rules hStateType hStateType]
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
perh::(Eq hStateType,Eq stateType) => HMM stateType hStateType->stateType->hStateType->Float
perh hmm y x = let lst = get1to hmm y x in
				if lst == [] then 1.0
				else cost.head$ lst
-- get2to hmm y x; -- получили правило перехода из скрытого состояния x в y

perh2 :: (Eq hStateType,Eq stateType) =>HMM stateType hStateType -> hStateType -> hStateType -> Float
perh2 hmm y x = let lst = get2to hmm y x in
				if lst == [] then 1.0
				else cost.head$ lst

perh3::(Eq hStateType,Eq stateType) => HMM stateType hStateType->hStateType->Float
perh3 hmm x =  let lst = get3to hmm x in
				if lst == [] then 1.0
				else cost.head$ lst

perhs :: (Eq stateType, Ord hStateType) => HMM stateType hStateType -> [stateType] -> (Float, hStateType)
perhs hmm (x:[]) = maximum $ map (\el -> ((perh hmm x el)* (perh3 hmm el),el)) (hstates hmm)
perhs hmm (x:str) = maximum $ map (\el -> let 
									(a,b) = (perhs hmm str);
									pb2el = (perh2 hmm el b);
									res = (perh hmm x el)*pb2el*a;
									in(res,el)) (hstates hmm)

-- perhs0 hmm (x:[]) =  maximum $ map (\el -> ((perh hmm x el)* (perh3 hmm el),el)) (hstates hmm)
{- tblHMM::HMM Char Char
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
lab5 hmm str = reverse$map (\x -> perhs hmm x) (init$tails$reverse str) -}