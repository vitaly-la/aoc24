import Data.Char (digitToInt)
import Data.List (sort)

solve xs =
    let addIdx (x:xt) = scanl (\(idx, y) z -> (idx + y, z)) (0, x) xt
        files = [(idx, sz, div i 2) | (i, (idx, sz)) <- zip [0..] . addIdx $ xs, even i]
        gaps = [x | (i, x) <- zip [0..] . addIdx $ xs, odd i]

        solve' [] _ ans = ans
        solve' ((idx, sz, fid):ft) gaps ans =
            let gaps' = filter (\(idx', sz') -> idx' < idx && sz <= sz') gaps

                ans' = case gaps' of
                    ((idx', _):_) -> ans + fid * sum [idx'..idx' + sz - 1]
                    _ -> ans + fid * sum [idx..idx + sz - 1]

                gaps'' = case gaps' of
                    (gap@(idx', sz'):_) -> sort $ (idx' + sz, sz' - sz) : filter (/= gap) gaps
                    _ -> gaps

            in solve' ft gaps'' ans'

    in solve' (reverse files) gaps 0

main = do
    contents <- getContents
    print . solve . map digitToInt . head . lines $ contents
