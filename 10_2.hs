import Data.Char (digitToInt)
import Data.Map ((!))
import qualified Data.Map as M (empty, fromList, insert)

solve contents =
    let parsed = [map digitToInt line | line <- lines contents]

        width = length $ parsed !! 0
        height = length parsed

        terrain = M.fromList [((x, y), h) | (y, line) <- zip [0..] parsed, (x, h) <- zip [0..] line]

        inside (x, y) = 0 <= x && x < width && 0 <= y && y < height
        uphill v v' = terrain ! v' - terrain ! v == 1

        addVert (x, y) graph = M.insert (x, y) (filter (uphill (x, y)) . filter inside $ [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]) graph
        graph = foldr addVert M.empty [(x, y) | x <- take width [0..], y <- take height [0..]]

        dfs' [] ans = ans
        dfs' (v:vt) ans = dfs' (vt ++ graph ! v) $ ans + if terrain ! v == 9 then 1 else 0
        dfs v = dfs' [v] 0

    in sum [dfs (x, y) | x <- take width [0..], y <- take height [0..], terrain ! (x, y) == 0]

main = do
    contents <- getContents
    print . solve $ contents
