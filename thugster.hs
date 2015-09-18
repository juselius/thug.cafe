import Data.List
import Data.Char
import Data.Bits
import Numeric

encr = concatMap (flip showHex "" . xor 0xff . ord)
    . unwords
    . map reverse
    $ words "Join now and become a thugster today!"

decr = foldl ((. reverse) . (++)) mempty
    . intersperse [chr $ 2^5]
    . words
    . map (chr . xor 0xff . fst . head . readHex)
    . takeWhile (not . null) $ unfoldr (Just . splitAt 2) encr

main = do
    putStrLn encr
    putStrLn decr
