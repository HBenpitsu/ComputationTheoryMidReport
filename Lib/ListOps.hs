module Lib.ListOps where

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace 0 new (_:xs) = new : xs
replace n new (x:xs) = x : replace (n - 1) new xs

insert :: Int -> a -> [a] -> [a]
insert 0 new xs = new : xs
insert _ new [] = [new]
insert n new (x:xs) = x : insert (n - 1) new xs

indexOf :: Eq a => a -> [a] -> Maybe Int
indexOf _ [] = Nothing
indexOf t (x:xs) = do 
    if t == x 
        then Just 0 
        else case indexOf t xs of
            Nothing -> Nothing
            Just idx -> Just (idx + 1)