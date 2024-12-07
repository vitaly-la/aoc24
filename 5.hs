import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Set (empty, insert, member)

cmpFun cmp x y = if member (x, y) cmp then LT else if member (y, x) cmp then GT else EQ

solve (cmp, ans) line = case splitOn "|" line of
    [x, y] -> (insert (x, y) cmp, ans)
    _ -> case splitOn "," line of
        [_] -> (cmp, ans)
        xs -> (cmp, 
            let sorted = sortBy (cmpFun cmp) xs
                middle = sorted !! (length sorted `div` 2)
            in if xs == sorted then middle:ans else ans)

main = do
    contents <- getContents
    print . sum . map read . snd $ foldl solve (empty, []) $ lines contents
