import Data.List (groupBy, sort)
import Data.Set (fromList, size)

enum = zip [0..]
combinations xs = concat . map (zip xs) . drop 1 $ scanr (:) [] xs

solve lines =
    let width = length $ lines !! 0
        height = length lines

        antennas = [(type', x, y) | (y, line) <- enum lines, (x, type') <- enum line, type' /= '.']
        byType = map (map (\(_, x, y) -> (x, y))) $ groupBy (\(type', _, _) (type'', _, _) -> type' == type'' ) $ sort antennas
        pairs = concat $ map combinations byType

        getNodes ((x1, y1), (x2, y2)) =
            let (xdiff, ydiff) = (x2 - x1, y2 - y1)
            in filter (\(x, y) -> x >= 0 && y >= 0 && x < width && y < height) [(x1 - xdiff, y1 - ydiff), (x2 + xdiff, y2 + ydiff)]
    in fromList . concat $ map getNodes pairs

main = do
    contents <- getContents
    print . size . solve $ lines contents
