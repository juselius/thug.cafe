import Data.List
import Data.Char
import Data.List.Split
import Data.Bits
import Numeric

encr = concat . map (flip showHex "" . xor 0xff . ord)
    . unwords
    . map reverse
    $ words "Join now and become a thugster today!"

decr = foldl ((. reverse) . (++)) mempty
    . intersperse [chr $ 2^5]
    . words
    . map (chr . xor 0xff . fst . head . readHex)
    $ chunksOf 2 encr

main = do
    putStrLn encr
    putStrLn decr
