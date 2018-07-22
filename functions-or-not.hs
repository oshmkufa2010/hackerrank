import Data.Map.Lazy (fromListWith, elems)
import Data.List (nub)
import Control.Monad (forM, forM_)

main :: IO ()
main = do
    t <- readLn :: IO Int
    
    forM_ [1..t] $ \i -> do
        n <- readLn :: IO Int
        
        coors <- forM [1..n] $ \j -> (fmap $ map (\w -> read w :: Int) . words) $ getLine
        
        let result = and $ map (\vs -> length vs == 1) $ elems $ fromListWith (\a b -> nub (a ++ b)) $ map (\[x, y] -> (x, [y])) coors 
        if result then putStrLn "YES" else putStrLn "NO" 
        
        
 