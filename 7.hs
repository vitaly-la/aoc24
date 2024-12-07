import Data.List.Split (splitOn)

op3 x y = read $ show x ++ show y

solutions res (x:xt) =
    foldl (
        \ys z -> filter (<= res) $ concat [[op y z | y <- ys] | op <- [(+), (*), op3]]
    ) [x] xt

solve line ans =
    let [res_str, eq] = splitOn ": " line
        res = read res_str
        xs = map read $ splitOn " " eq
    in if elem res $ solutions res xs then res:ans else ans

main = do
    contents <- getContents
    print . sum . foldr solve [] $ lines contents
