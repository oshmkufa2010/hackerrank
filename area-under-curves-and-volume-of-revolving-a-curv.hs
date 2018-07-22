import Text.Printf (printf)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r as bs = map reduceBy [id, (\h -> 3.14159265354 * h ** 2)]
    where reduceBy g = sum $ map ((*) 0.001 . g . (\i -> (f ((i + 1) / 1000))) . fromIntegral) [l*1000..r*1000-1]
          f x = sum $ map (\(a, b) -> a * x ** b) $ map (\(a, b) -> (fromIntegral a, fromIntegral b)) $ zip as bs


--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines