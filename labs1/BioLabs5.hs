module BioLabs5(
		)
where
-- import Data.List
import Data.Ord
import Data.String.Utils
import System.IO
import qualified Data.ByteString as B
-- import Data.ByteString

lcstr0B xs ys = B.map (B.reverse) 
			$union []
			$B.filter(/="") 
			$B.concat 
 $[f xs' ys | xs' <- B.tails xs] ++ [f xs ys' | ys' <- B.tail $ B.tails ys]
  where f xs ys = scanl g [] $ B.zip xs ys
        g z (x, y) = if x == y then x:z else []
		
vrfyItB:: [B.ByteString] -> [B.ByteString] -> B.ByteString
vrfyItB strs [] = maximumBy (comparing B.length) strs
vrfyItB strs (el:lst)= let
				strs2 = union [] $B.concat$ map (\x -> lcstr0B x el) strs
			in vrfyItB strs2 lst
			
vrfyItWithFireB :: [B.ByteString] -> B.ByteString
vrfyItWithFireB (el:el2:lst) = vrfyItB (lcstr0B el el2) lst