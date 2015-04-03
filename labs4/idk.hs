{- Давайте в качестве 5й задачи Вы сделаете свою HMM, 
но только с витерби (максимальный путь), без вывода параметров.
-}

import Data.List

data Cluster = Cluster{ len :: Int
					 , seqs::[String]
					 } deriving (Show)

makeClusters::[String]->[Cluster]
makeCluster strs = let
			f :: [String]->[[String]]->[Int]->[Cluster]
			f [] res nums = zipWith (\x y-> Cluster y x)
			f allStr res nums =
			in result
{-
есть операции
-- замена
-- вставка
-- удаление 
Учтём ограничение:
Алфавит ТОЛЬКО 4 буквы
-}