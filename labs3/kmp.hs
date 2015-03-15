-- https://ru.wikibooks.org/wiki/%D0%A0%D0%B5%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D0%B8_%D0%B0%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC%D0%BE%D0%B2/%D0%90%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC_%D0%9A%D0%BD%D1%83%D1%82%D0%B0_%E2%80%94_%D0%9C%D0%BE%D1%80%D1%80%D0%B8%D1%81%D0%B0_%E2%80%94_%D0%9F%D1%80%D0%B0%D1%82%D1%82%D0%B0
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString.Char8 as BS
 
type PrefixTable = V.Vector Int
{-
makePrefixTable (BS.pack "ABCABD")
fromList [0,0,0,1,2,0]

Долго разбирался
-}
makePrefixTable :: BS.ByteString -> PrefixTable
makePrefixTable s =
  let n = BS.length s
  --  create (do { v <- new 2; write v 0 'a'; write v 1 'b' }) = <a,b>
  in V.create $ do
    p <- VM.new n
    VM.set p 0 -- установить в p значения всех элементов в 0
    let loop i k | i == n = return ()
				 -- if k>0 && s[i] != s[k]
                 | k > 0 && (BS.index s i /= BS.index s k) = do
                     nk <- VM.read p (k - 1) -- nk = p[k-1]
                     loop i nk
                 | otherwise = do
                     let nk = if BS.index s i == BS.index s k
                              then k + 1
                              else k
                     VM.write p i nk -- записать в p на i-ю позицию значение nk
                     loop (i + 1) nk 
    loop 1 0
    return p
 
indexOf :: BS.ByteString -> BS.ByteString -> Int
indexOf needle haystack =
  let pt    = makePrefixTable needle
      hLen  = BS.length haystack
      nLen  = BS.length needle
      loop i k | k == nLen = i - nLen
               | i == hLen = -1
               | k > 0 && (BS.index needle k /= BS.index haystack i) =
                   loop i $ pt V.! (k - 1)
               | otherwise =
                   let nk = if BS.index needle k == BS.index haystack i
                            then k + 1
                            else k
                   in loop (i + 1) nk
  in loop 0 0