module Golf where

-- Exercise 01
-- Hopscotch

skips :: [a] -> [[a]]
skips xs = map (\n -> nth n xs) [1..l]
  where l = length xs
        nth n xs = map snd $ filter (\(i, x) -> i `rem` n == 0) $ zip [1..l] xs
