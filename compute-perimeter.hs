import Control.Monad (forM, forM_)

main :: IO ()
main = do
    n <- readLn :: IO Int
    coors <- forM [1..n] $ \_ -> (fmap $ (\(a:b:_) -> (a, b)) . map (fromIntegral . (\w -> read w :: Int)) . words) $ getLine
    let result = sum $ map (\((x1, y1), (x2, y2)) -> sqrt $ (x1-x2)**2 + (y1-y2)**2) $ zip ((last coors) : (init coors)) coors
    putStrLn $ show result 