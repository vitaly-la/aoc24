import Data.Char (digitToInt)

blocks _ _ [] = []
blocks file idx (x:xt)
    | file = take x $ repeat idx ++ blocks False idx xt
    | otherwise = take x $ repeat (-1) ++ blocks True (idx + 1) xt

solve xs =
    let enum = zip [0..] xs
        rev = filter ((/= -1) . snd) $ reverse enum
        solve' [] _ = 0
        solve' ((ix, x):xt) (ys@((iy, y):yt))
            | ix > iy = 0
            | x == -1 = ix * y + solve' xt yt
            | otherwise = ix * x + solve' xt ys
    in solve' enum rev

main = do
    contents <- getContents
    print . solve . blocks True 0 . map digitToInt . head $ lines contents
